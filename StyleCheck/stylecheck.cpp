#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Rewrite/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"

#include <iostream>
#include <regex>
#include <set>

#include "issue.hpp"



#include "clang/ASTMatchers/ASTMatchersInternal.h"
#include "clang/ASTMatchers/ASTMatchersMacros.h"

namespace clang {
  namespace ast_matchers {
    const internal::VariadicDynCastAllOfMatcher<Stmt, ParenExpr> parenExpr;
    const internal::VariadicDynCastAllOfMatcher<Stmt, UnresolvedLookupExpr> unresolvedLookupExpr;

    AST_MATCHER(FunctionDecl, isDefaulted) { return Node.isDefaulted(); }
    AST_MATCHER(UnresolvedLookupExpr, isSystemURE) {
      std::string callee = Node.getName().getAsString();
      //std::cerr << "CALLEE: " << callee << std::endl;
      // Check for a __ prefix
      std::string uu = "__";
      if (callee.size() < 2) return false;
      return std::mismatch(uu.begin(), uu.end(), callee.begin()).first == uu.end();
    }
  }
}

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
using namespace std;
static cl::OptionCategory StyleCheckCategory("Style Check Options");

static cl::opt<bool> Force ("f", cl::desc("Overwrite output files"));
static cl::opt<bool> HTMLoutput("html",
                                cl::cat(StyleCheckCategory),
                                cl::desc("Generate HTML output"));

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
//static cl::extrahelp MoreHelp("\nMore help text...");


std::set<Issue> lineIssues;


std::regex is_camelCase = std::regex("(.*::)?[a-z][a-z0-9_]*([A-Z][a-zA-Z0-9_]*)*");
std::regex is_CamelCase = std::regex("(.*::)?[A-Z][a-z0-9_]*([A-Z][a-zA-Z0-9_]*)*");
std::regex final_underscore = std::regex(".*_");
std::regex is_operator = std::regex("(.*::)?(operator[^A-Za-z]+|operator .*)");
std::regex is_UPPER_CASE = std::regex("(.*::)?[A-Z][A-Z0-9]*(_[A-Z0-9]+)*");

// Clang introduces variables like __range to implement foreach loops
std::regex is_internal = std::regex("__[A-Za-z0-9]*");

std::regex is_headerFile = std::regex(".*\\.h(pp)?");


/////////////////////////////
// Cyclomatic Complexity
/////////////////////////////

class Cyclomatic : public RecursiveASTVisitor<Cyclomatic> {
public:
  bool VisitForStmt(ForStmt*) {
    ++count_;
    return true;
  }
  bool VisitIfStmt(IfStmt*) {
    ++count_;
    return true;
  }
  bool VisitWhileStmt(WhileStmt*) {
    ++count_;
    return true;
  }
  bool VisitSwitchStmt(SwitchStmt*) {
    ++count_;
    return true;
  }
  bool VisitCXXCatchStmt(CXXCatchStmt*) {
    ++count_;
    return true;
  }
  bool VisitBinaryOperator(BinaryOperator* b) {
    auto op = b->getOpcode();
    if (op == BO_LAnd || op == BO_LOr) ++count_;
    return true;
  }

  size_t getComplexity() { return count_; }
private:
  size_t count_ = 1;
};

///////////////////////////////
// Callbacks for Nodes Found
///////////////////////////////


void addIssue(const SourceManager& SM, const SourceRange& range,
              std::set<Issue>& v, std::string message, Severity sev = Severity::WARNING)
{
  SourceLocation start = range.getBegin();
  SourceLocation stop  = range.getEnd();

  // Adapted from http://lists.cs.uiuc.edu/pipermail/cfe-commits/Week-of-Mon-20121022/066863.html
  while (start.isMacroID())
      start = SM.getImmediateMacroCallerLoc(start);

  FileID LocFileID = SM.getFileID(start);

   while (stop.isMacroID() && SM.getFileID(stop) != LocFileID) {
      // The computation of the next End is an inlined version of
      // getImmediateMacroCallerLoc, except it chooses the end of an
      // expansion range.
      if (SM.isMacroArgExpansion(stop)) {
        stop = SM.getImmediateSpellingLoc(stop);
      } else {
        stop = SM.getImmediateExpansionRange(stop).second;
      }
    }

    start = SM.getSpellingLoc(start);
    stop = SM.getSpellingLoc(stop);

  size_t offset = Lexer::MeasureTokenLength(stop, SM, LangOptions());
  std::string code = "...code not available...";

  //if (SM.isMacroArgExpansion(start)) start = SM.getFileLoc(start);
  //if (SM.isMacroArgExpansion(stop )) stop  = SM.getFileLoc(stop);

  if (SM.isWrittenInSameFile(start, stop)) {
    ptrdiff_t len = SM.getCharacterData(stop)-SM.getCharacterData(start)+offset;
    if (len >= 0 && len < 1000) {
      code = std::string(SM.getCharacterData(start), len);
    }
  }

  v.emplace( SM.getFilename(start),
             SM.getPresumedLineNumber(start),
             SM.getPresumedColumnNumber(start),
             message, code, sev );

}




// Each class defines a run() method handling matches
//    for a different rule.
// For simplicity, we declare and define all the code
//    of the class in one place.


class ProcessDecl : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const Decl *decl = Result.Nodes.getNodeAs<Decl>("decl");

    //decl->dump();

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = decl->getSourceRange();

    switch (decl->getKind()) {
      case Decl::UsingDirective:
        {
          std::string filename = SM.getFilename(range.getBegin());
          if (std::regex_match(filename, is_headerFile)) {
            addIssue( SM, range, lineIssues,
                      "Found 'using namespace' in a header" );
          }
          break;
        }

      case Decl::Function:
      case Decl::CXXMethod:
        {
          const FunctionDecl* f = dyn_cast<FunctionDecl>(decl);
          if (! f->doesThisDeclarationHaveABody()) break;

          std::string name = f->getNameAsString();
          Stmt* body = f->getBody();

          range.setEnd(body->getLocStart());

          bool matches_camelCase = std::regex_match(name, is_camelCase);
          bool matches_operator = std::regex_match(name, is_operator);
          bool has_underscore = std::regex_match(name, final_underscore);

          if ( (! matches_camelCase || has_underscore) &&
               ! matches_operator) {
            addIssue( SM, range, lineIssues,
                      "Found non-camelCase function name: " + name);
          }

          Cyclomatic cyc;
          cyc.TraverseStmt(body);
          auto complexity = cyc.getComplexity();
          if ( complexity > 10) {
             addIssue (SM, range, lineIssues,
                        "Function " + name + " has complexity " + to_string(complexity) +
                        "; aim for below 10 (15 in extremity).");
          }
          break;
        }

      case Decl::Var:
        {
          const VarDecl* f = dyn_cast<VarDecl>(decl);
          std::string name = f->getDeclName().getAsString();
          bool matches_camelCase = std::regex_match(name, is_camelCase);
          bool has_underscore = std::regex_match(name, final_underscore);
          bool matches_UPPER_CASE = std::regex_match(name, is_UPPER_CASE);
          bool matches_internal = std::regex_match(name, is_internal);
          auto varType = f->getType();
          bool is_const = varType.isConstQualified();
          const Expr* init = f->getAnyInitializer();
          bool has_constexpr_definition =
            (f->isConstexpr()) ||
            // The following code crashes with assertion errors as of July 2, 2015
            //((init != nullptr) && (init->isCXX11ConstantExpr(*(Result.Context))));
            false;

          if (!is_const &&
              ! (matches_camelCase || has_underscore) &&
              ! (matches_camelCase && has_underscore && f->isStaticDataMember()) &&
              ! matches_internal) {
            addIssue( SM, range, lineIssues,
                      "Found a non-camelCase variable name: " +
                        f->getQualifiedNameAsString() +
                        ((matches_UPPER_CASE && has_constexpr_definition) ?
                           " (is this supposed to be constant?)" : "") );
          } else if (is_const &&
                      ! matches_UPPER_CASE &&
                      ! (matches_camelCase && !has_constexpr_definition) &&
                      ! matches_internal) {
            addIssue( SM, range, lineIssues,
                      "Found a non-UPPER_CASE constant name: " +
                        f->getQualifiedNameAsString() );
          }
          break;
        }

      case Decl::Field:
        {
          const FieldDecl* f = dyn_cast<FieldDecl>(decl);
          std::string name = f->getNameAsString();
          bool matches_camelCase = std::regex_match(name, is_camelCase);
          bool has_underscore = std::regex_match(name, final_underscore);

          bool matches_UPPER_CASE = std::regex_match(name, is_UPPER_CASE);
          auto varType = f->getType();
          bool is_const = varType.isConstQualified();

          if (!matches_camelCase || !has_underscore) {
            addIssue( SM, range, lineIssues,
                      "Found a non-camelCase_ data member: " +
                        f->getQualifiedNameAsString() +
                        (is_const && matches_UPPER_CASE ?
                           " (should this be a 'static constexpr' data member?)" : "" ));
          }
          break;
        }

      case Decl::Record:
        // struct/union/class
        {

          const CXXRecordDecl* f = dyn_cast<CXXRecordDecl>(decl);
          if (! f->isCompleteDefinition()) break;

          std::string name = f->getNameAsString();
          bool matches_CamelCase = std::regex_match(name, is_CamelCase);
          bool has_underscore = std::regex_match(name, final_underscore);

          if (!matches_CamelCase || !has_underscore) {
            std::string kind = f->isClass() ? "class" :
                                 (f->isStruct() ? "struct" : "union");
            addIssue( SM, range, lineIssues,
                      "Found a non-CamelCase " + kind + " name: " +
                        f->getQualifiedNameAsString() );
          }
          break;
        }

    default:
        break;

/*
      case Decl::Block:
        break;


      case Decl::CXXConstructor:
        break;

      case Decl::CXXDestructor:
        break;

      case Decl::CXXConversion:
        break;

      case Decl::Block:
        break;

      case Decl::TemplateTypeParm:
        break;

      case Decl::VarTemplate:
        break;

      case Decl::FunctionTemplate:
        break;

      case Decl::TypeAliasTemplate:
        break;

      case Decl::TemplateTemplateParm:
        break;

      case Decl::Friend:
      case Decl::FriendTemplate:
        break;

      case Decl::Captured:
        break;


      case Decl::Enum:
      case Decl::EnumConstant:
        break;

      case Decl::Label:
        break;

      case Decl::NamespaceAlias:
        break;

      case Decl::Using:
        // using X;
        break;


      case Decl::Typedef:
      case Decl::TypeAlias:
        break;

      default:
        break;
        */
    }
  }
};


class ProcessMemberCall : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const CXXMemberCallExpr *expr = Result.Nodes.getNodeAs<CXXMemberCallExpr>("callWithPointer");

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue( SM, range, lineIssues,
        "Found a call using (*foo).bar(...) notation instead of foo->bar(...) notation");
  }
};

class ProcessMemberProj : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const MemberExpr *expr = Result.Nodes.getNodeAs<MemberExpr>("projWithPointer");

    //expr->dump();

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue( SM, range, lineIssues,
        "Using '->' would be better!");
  }
};


class ProcessGotoStmt : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const GotoStmt *decl = Result.Nodes.getNodeAs<GotoStmt>("goto");

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = decl->getSourceRange();

    addIssue( SM, range, lineIssues, "Go To statement considered harmful.");
  }
};

/*
class ProcessThisCall : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched `this' AST node, NOT the whole this->foo expression!
    const CXXThisExpr* thisExpr = Result.Nodes.getNodeAs<CXXThisExpr>("myThis");
    const CXXMemberCallExpr* callExpr = Result.Nodes.getNodeAs<CXXMemberCallExpr>("myCall");


    if (thisExpr->isImplicit()) {
        // Don't yell at the user for implicit `this'!
        return;
    }


    callExpr->dump();

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = callExpr->getSourceRange();

    addIssue( SM, range, lineIssues, "Explicit use of this->foo() instead of foo()");
  }
};
*/

class ProcessThisProj : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched `this' AST node, NOT the whole this->foo expression!
    const CXXThisExpr* thisExpr = Result.Nodes.getNodeAs<CXXThisExpr>("myThis");
    const MemberExpr* projExpr = Result.Nodes.getNodeAs<MemberExpr>("projection");

    if (thisExpr->isImplicit()) {
        // Don't yell at the user for implicit `this'!
        return;
    }

    //projExpr->dump();

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = projExpr->getSourceRange();

    std::string fieldName = projExpr->getMemberNameInfo().getAsString();

    addIssue( SM, range, lineIssues,
        "Expression could be simplified to just '" + fieldName + "'");
  }
};

class ProcessIncDec : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched `this' AST node, NOT the whole this->foo expression!
    const UnaryOperator* incExpr = Result.Nodes.getNodeAs<UnaryOperator>("increment");

    if (incExpr->isPrefix()) return;

    std::string operation =
      (incExpr->isIncrementOp() ? "increment" : "decrement");

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = incExpr->getSourceRange();

    addIssue( SM, range, lineIssues,
        "Pre-" + operation + " is preferred to post-" + operation + " in idiomatic C++");
  }
};

// XXX: Complains about #defined constants such as RAND_MAX
class ProcessMagicNumbers : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched `this' AST node, NOT the whole this->foo expression!
    const Expr* expr = Result.Nodes.getNodeAs<Expr>("literal");

    //expr->dump();

    if (const IntegerLiteral* litExpr = Result.Nodes.getNodeAs<IntegerLiteral>("literal")) {
      auto value = litExpr->getValue();
      if (value == -1 || value == 0 || value == 1 || value == 2) return;
    } else if (const FloatingLiteral* fltExpr = Result.Nodes.getNodeAs<FloatingLiteral>("literal")) {
      auto value = fltExpr->getValueAsApproximateDouble();
      if (value == 0.0 || value == 1.0 || value == -1.0) return;
    } else {
      cerr << "CAN'T UNDERSTAND LITERAL!" << endl;
    }

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue( SM, range, lineIssues, "Is this a magic number?");
  }
};

class ProcessEqBool : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    const BinaryOperator* expr = Result.Nodes.getNodeAs<BinaryOperator>("expr");
    const CXXBoolLiteralExpr* boolLit = Result.Nodes.getNodeAs<CXXBoolLiteralExpr>("bool");

    bool trueConst = boolLit->getValue();
    bool equality = expr->getOpcode() == BO_EQ;

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    if (trueConst) {
      if (equality) {
        addIssue( SM, range, lineIssues, "Redundant (or buggy!) \"== true\"");
      } else {
        addIssue( SM, range, lineIssues, "The prefix ! (not) operator would be more idiomatic");
      }
    } else {
      if (equality) {
        addIssue( SM, range, lineIssues, "The prefix ! (not) operator would be more idiomatic");
      } else {
        addIssue( SM, range, lineIssues, "Redundant \"!= false\"");
      }
    }
  }
};

class ProcessNull : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    const Expr* expr = Result.Nodes.getNodeAs<Expr>("expr");

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue( SM, range, lineIssues, "Modern C++ uses nullptr");
  }
};

class ProcessCCast : public MatchFinder::MatchCallback {
public:
  virtual void run(const MatchFinder::MatchResult &Result) {
    const Expr* expr = Result.Nodes.getNodeAs<Expr>("expr");

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue( SM, range, lineIssues, "Modern C++ avoids C-style casts");
  }
};
////////////////////////
//  main
///////////////////////
//
int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  CommonOptionsParser OptionsParser(argc, argv, StyleCheckCategory);
  tooling::ClangTool Tool(OptionsParser.getCompilations(),
                          OptionsParser.getSourcePathList());
  MatchFinder Finder;

  // Create callback object(s) for each rule.
  ProcessMemberCall cb5;
  ProcessMemberProj cb9;
  ProcessGotoStmt cb6;
//  ProcessThisCall  cb7;
  ProcessThisProj  cb8;
  ProcessDecl  cbDecl;
  ProcessIncDec cb10;
  ProcessMagicNumbers cb11;
  ProcessEqBool cb12;
  ProcessNull cb13;
  ProcessCCast cb14;

  Finder.addMatcher(
      // Look for invocations (*p).method(...)
      //  But only where p is a pointer. Otherwise, it _might_ be reasonable.
      //  (e.g., if p belongs to a class that did not implement ->.)
      memberCallExpr(unless(isExpansionInSystemHeader()),
                     on(unaryOperator(hasOperatorName("*"),
                                      hasUnaryOperand(expr(hasType(pointerType())))))
          ).bind("callWithPointer"),
      &cb5);

  Finder.addMatcher(
      // Look for invocations (*p).field_
      //  But only where p is a pointer. Otherwise, it _might_ be reasonable.
      //  (e.g., if p belongs to a class that did not implement ->.)
      memberExpr(
          unless(isExpansionInSystemHeader()),
          hasObjectExpression(
             parenExpr(
               has(
                 unaryOperator(
                   hasOperatorName("*"),
                   hasUnaryOperand(
                     expr(unless(thisExpr()),
                          hasType(pointerType())
                     )
                   )
                 )
               )
             )
           )
         ).bind("projWithPointer"),
      &cb9);

  Finder.addMatcher(
      gotoStmt(unless(isExpansionInSystemHeader())).bind("goto"),
      &cb6);
/*
  // Seems to be handled by the MemberExpr catcher.
  Finder.addMatcher(
      memberCallExpr(
          unless(isExpansionInSystemHeader()),
          on(anyOf(thisExpr().bind("myThis"),
                   unaryOperator(hasOperatorName("*"),
                                 hasUnaryOperand(thisExpr().bind("myThis")))))
          ).bind("myCall"),
      &cb7);
*/

  Finder.addMatcher(
      memberExpr(
          unless(isExpansionInSystemHeader()),
          hasObjectExpression(
             parenExpr(
               has(
                 unaryOperator(
                   hasOperatorName("*"),
                   hasUnaryOperand(
                     thisExpr().bind("myThis")
                   )
                 )
               )
             )
           ),
          // Some implicitly-generated code seems to use "this->"
          unless(hasAncestor(isImplicit())),
          unless(hasAncestor(functionDecl(isDefaulted())))
         ).bind("projection"),
      &cb8);


  Finder.addMatcher(
      memberExpr(
          hasObjectExpression(thisExpr().bind("myThis")),
          unless(isExpansionInSystemHeader()),
          // Some implicitly-generated code seems to use this->
          unless(hasAncestor(isImplicit())),
          // Code generated by =default  seems to use this->
          unless(hasAncestor(functionDecl(isDefaulted())))
          ).bind("projection"),
      &cb8);

  Finder.addMatcher(
      // ignore declarations in expanded templates, for now.
      // Otherwise, we get errors about names in templates both it they are defined
      //    *and* when they are (usually implicitly) instantiated
      decl(unless(anyOf(isExpansionInSystemHeader(),
                        isInstantiated(),
                        hasParent(catchStmt())))).bind("decl"),
      &cbDecl);

  Finder.addMatcher(
       unaryOperator(
          unless(isExpansionInSystemHeader()),
          anyOf(hasOperatorName("++"),hasOperatorName("--")),
          unless(hasParent(expr()))
         ).bind("increment"),
      &cb10);

  Finder.addMatcher(
      integerLiteral(
         unless(equals(0)),  // Optimization?
         unless(equals(1)),  // Optimization?
         unless(isExpansionInSystemHeader()),
         unless(hasParent(decl())),
         unless(hasParent(implicitCastExpr(hasParent(decl())))),
         // Things like assert() create constant line numbers.
         unless(hasAncestor(callExpr(callee(namedDecl(matchesName("__")))))),
         // Sometimes assert beomces an unresolvedLookupExpr
         unless(hasAncestor(callExpr(callee(unresolvedLookupExpr(isSystemURE()))))),
         // e.g., the constants in primeLessthan
         unless(hasParent(initListExpr()))
        ).bind("literal"),
      &cb11);

  Finder.addMatcher(
      floatLiteral(
         unless(equals(0.0)),  // Optimization?
         unless(equals(1.0)),  // Optimization?
         unless(isExpansionInSystemHeader()),
         unless(hasParent(decl())),
         unless(hasParent(implicitCastExpr(hasParent(decl())))),
         // e.g., the constants in primeLessthan
         unless(hasParent(initListExpr()))
        ).bind("literal"),
      &cb11);


  Finder.addMatcher(
      binaryOperator(
         unless(isExpansionInSystemHeader()),
         anyOf(hasOperatorName("=="),
               hasOperatorName("!=") ),
         anyOf(hasLHS(ignoringImpCasts(boolLiteral().bind("bool"))),
               hasRHS(ignoringImpCasts(boolLiteral().bind("bool"))) )
        ).bind("expr"),
      &cb12);

  Finder.addMatcher(
      declRefExpr(
        unless(isExpansionInSystemHeader()),
        to(varDecl(hasName("NULL")))
       ).bind("expr"),
      &cb13);

  Finder.addMatcher(
      gnuNullExpr(
        unless(isExpansionInSystemHeader())
       ).bind("expr"),
      &cb13);

  Finder.addMatcher(
      implicitCastExpr(
        has(integerLiteral(equals(0))),
        unless(isExpansionInSystemHeader()),
        hasImplicitDestinationType(pointerType())
       ).bind("expr"),
      &cb13);

  Finder.addMatcher(
      cStyleCastExpr(
        unless(isExpansionInSystemHeader()),
        // the assert macro seems to do the cast (void)0
        unless(has(integerLiteral(equals(0))))
       ).bind("expr"),
      &cb14);

  // Run everything. If we suggested doing any replacements,
  // save the changes to disk
  Tool.run(newFrontendActionFactory(&Finder).get());

  // lineissues is now a set, not a vector.
  // Sort the issues by line number
  //   std::sort(lineIssues.begin(), lineIssues.end());
  // Remove duplicates (inefficiently!)
  //   lineIssues.erase( unique( lineIssues.begin(), lineIssues.end() ), lineIssues.end() );

  //Produce either terminal or HTML output
  if (HTMLoutput) {
    for (const auto& issue: lineIssues) {
      cout << issue.getHTML() << endl;
    }
  } else {
    cout << endl;
    for (const auto& issue: lineIssues) {
      cout << issue.getText() << endl;
    }
  }

}

