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
#include <set>

// #include <boost/regex.hpp>
#include <regex>

#include "issue.hpp"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
//using namespace std;

////////////////////////////
// AST Matchers
////////////////////////////

// As mentioned in the "AST Matcher Reference"
//   http://clang.llvm.org/docs/LibASTMatchersReference.html
// there aren't predefined matchers for all sorts of AST nodes,
// or for all properties of AST nodes.
//
// Fortunately, it turns out that it's not too hard to
// add our own.

#include "clang/ASTMatchers/ASTMatchersInternal.h"
#include "clang/ASTMatchers/ASTMatchersMacros.h"

#if 0
namespace clang {
  namespace ast_matchers {

    // Node matcher: a parenthesized expression
    const internal::VariadicDynCastAllOfMatcher<Stmt, ParenExpr> parenExpr;

    // Node matcher: an UnresolvedLookupExpr (unresolved variable)
    const internal::VariadicDynCastAllOfMatcher<Stmt, UnresolvedLookupExpr> unresolvedLookupExpr;

    // Narrowing matcher: is the function one created by "= default" ?
    AST_MATCHER(FunctionDecl, isDefaulted) { return Node.isDefaulted(); }

    // Narrowing matcher: does the name of the UnresolvedLookupExpr
    //  start with "__", suggesting that it's a system helper function
    //  like __assert_ret ?
    AST_MATCHER(UnresolvedLookupExpr, isSystemURE) {
      std::string callee = Node.getName().getAsString();
      // Check for a __ prefix
      std::string uu = "__";
      if (callee.size() < 2) return false;
      return std::mismatch(uu.begin(), uu.end(), callee.begin()).first == uu.end();
    }

  }
}
#endif

///////////////////////////
// Command-line Options
///////////////////////////

static cl::OptionCategory StyleCheckCategory("Style Check Options");

static cl::opt<bool> HTMLoutput("html",
                                cl::cat(StyleCheckCategory),
                                cl::desc("Generate HTML output"));

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
//static cl::extrahelp MoreHelp("\nMore help text...");

///////////////
// Globals
///////////////

// The collection of problems found.
std::set<Issue> lineIssues;

// Various regular expressions for variable names
const std::regex is_camelCase("(.*::)?[a-z][a-z0-9_]*([A-Z][a-zA-Z0-9_]*)*");
const std::regex is_CamelCase("(.*::)?[A-Z][a-z0-9_]*([A-Z][a-zA-Z0-9_]*)*");
const std::regex final_underscore(".*_");
const std::regex is_operator("(.*::)?(operator[^A-Za-z]+|operator .*)");
const std::regex is_UPPER_CASE("(.*::)?[A-Z][A-Z0-9]*(_[A-Z0-9]+)*");
const std::regex is_UPPER_CASE_("(.*::)?[A-Z][A-Z0-9]*(_[A-Z0-9]+)*_");

// Clang introduces variables like __range to implement foreach loops
const std::regex is_internal("__[A-Za-z0-9]*");

// Does the filename end with .h or .hpp?
const std::regex is_headerFile(".*\\.h(pp)?");

// Numeric literals
const std::regex is_number{"[-+0-9.][-+0-9.Ee]*"};

/////////////////////////////
// Cyclomatic Complexity
/////////////////////////////

// A complexity of 10 or less is often recommended,
//     unless there's a good reason.
// But how high does the complexity need to be before we
//   want to start complaining here?
constexpr size_t COMPLEXITY_LIMIT = 12;

// For structured code, cyclomatic complexity is
//   defined to be one more than the number of
//   "decision points" in the code.
// Here we only count an n-way switch as 1 decision point.

// Write a recursive AST visitor that increments a counter
//   at each decision point.
class Cyclomatic : public RecursiveASTVisitor<Cyclomatic>
{
public:
  bool VisitForStmt(ForStmt *) { return inc(); }
  bool VisitIfStmt(IfStmt *) { return inc(); }
  bool VisitWhileStmt(WhileStmt *) { return inc(); }
  bool VisitSwitchStmt(SwitchStmt *) { return inc(); }
  bool VisitCXXCatchStmt(CXXCatchStmt *) { return inc(); }

  bool VisitBinaryOperator(BinaryOperator *b)
  {
    // Like inc(), but only count && and ||.
    auto op = b->getOpcode();
    if (op == BO_LAnd || op == BO_LOr)
      ++count_;
    return true;
  }

  size_t getComplexity() { return count_; }

private:
  size_t count_ = 1; // Cyclomatic complexity

  bool inc()
  {
    ++count_;    // Update the count
    return true; // Continue recursing through the tree.
  }
};

// Run the traversal on the given statement (function body)
//  (by creating a visitor object and starting it running)
//  and report the results.
size_t bodyComplexity(Stmt *body)
{
  Cyclomatic cyc;
  cyc.TraverseStmt(body);
  return cyc.getComplexity();
}

////////////////////////
// Reporting issues
///////////////////////

// Because of macros and templates, clang's "range"
// information about AST nodes is often flaky, e.g.,
// if you use a macro, the range for the *use*
// might span all the way from the definition of the
// macro to the end of the use.
//
// So, we do something more complex in going from
// the range object to the start/end pair.
//
std::pair<SourceLocation, SourceLocation>
getBounds(const SourceManager &SM, const SourceRange &range)
{
  SourceLocation start = range.getBegin();
  SourceLocation stop = range.getEnd();

  // Adapted from
  // http://lists.cs.uiuc.edu/pipermail/cfe-commits/Week-of-Mon-20121022/066863.html

  while (start.isMacroID())
    start = SM.getImmediateMacroCallerLoc(start);

  FileID LocFileID = SM.getFileID(start);

  while (stop.isMacroID() && SM.getFileID(stop) != LocFileID)
  {
    // The computation of the next End is an inlined version of
    // getImmediateMacroCallerLoc, except it chooses the end of an
    // expansion range.
    if (SM.isMacroArgExpansion(stop))
    {
      stop = SM.getImmediateSpellingLoc(stop);
    }
    else
    {
      stop = SM.getImmediateExpansionRange(stop).getEnd();
    }
  }

  start = SM.getSpellingLoc(start);
  stop = SM.getSpellingLoc(stop);

  return std::make_pair(start, stop);
}

// If a declaration is longer than this, we will
// not extract its code..
constexpr int MAX_CODE_LENGTH = 4096;

// Get the osurce code for the bounds.
// As a hack, allow an extra character offset
// for the end position.
std::string getCode(const SourceManager &SM,
                    SourceLocation start,
                    SourceLocation stop,
                    int offset = 0)
{
  std::string code = "...code not available...";

  // LLVM ranges tend to go from the start of the
  // first token in the text to the *start* of the
  // last token. So we calculate the length of the
  // last token.
  offset += Lexer::MeasureTokenLength(stop, SM, LangOptions());

  // If we did things right, the start and end are in the same file.
  if (SM.isWrittenInSameFile(start, stop))
  {
    ptrdiff_t len = SM.getCharacterData(stop) - SM.getCharacterData(start) + offset;
    if (len >= 0 && len < MAX_CODE_LENGTH)
    {
      // If len is negative, we didn't manage to get the right
      //   start/end pair.
      // If len is too big, either somethign went wrong
      code = std::string(SM.getCharacterData(start), len);
    }
  }

  return code;
}

constexpr auto DEFAULT_SEVERITY = Severity::WARNING;

// Add a problem to our collection of problems.
//
// The offset hack just gets passed to getCode.
void addIssue(const SourceManager &SM,
              const SourceRange &range,
              std::set<Issue> &v,
              std::string message,
              Severity sev = DEFAULT_SEVERITY,
              int offset = 0)
{
  auto bounds = getBounds(SM, range);
  SourceLocation start = bounds.first;
  SourceLocation stop = bounds.second;

  std::string code = getCode(SM, start, stop, offset);

  v.insert({SM.getFilename(start),
            SM.getPresumedLineNumber(start),
            SM.getPresumedColumnNumber(start),
            message, code, sev});
}

///////////////////////////////
// Callbacks for Nodes Found
///////////////////////////////

// Each class defines a run() method handling matches
//    for a different rule.
// For simplicity, we declare and define all the code
//    of the class in one place.

class ProcessDecl : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched declaration
    const Decl *decl = Result.Nodes.getNodeAs<Decl>("decl");
    assert(decl != nullptr);

    // Get the source location of that declaration
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = decl->getSourceRange();

    switch (decl->getKind())
    {

    case Decl::UsingDirective:
    {
      // "using namespace" Directive

      // Check that we're not in a header file.
      std::string filename = SM.getFilename(range.getBegin());
      if (std::regex_match(filename, is_headerFile))
      {
        addIssue(SM, range, lineIssues,
                 "Found 'using namespace' in a header");
      }
      break;
    }

    case Decl::Function:
    case Decl::CXXMethod:
    {
      // Function or Method
      const FunctionDecl *f = dyn_cast<FunctionDecl>(decl);

      // Complain about the definition, not the declaration.
      //  (Not all functions have separate declarations, and
      //  there's no obvious need to complain twice.)
      if (!f->doesThisDeclarationHaveABody())
        break;

      // Get the name and body of the function.
      std::string name = f->getNameAsString();
      Stmt *body = f->getBody();

      // We don't want to print the whole function
      // just because the name is funny. Move the
      // end of the displayed range to the start of the
      // function body.
      // range.setEnd(body->getLocStart());
      range.setEnd(body->getBeginLoc());

      // Check the name
      bool matches_camelCase = std::regex_match(name, is_camelCase);
      bool matches_operator = std::regex_match(name, is_operator);
      bool has_underscore = std::regex_match(name, final_underscore);

      // Note: unfortunately, the computed start/end
      //   pair for the function header is including the
      //   newline and opening curly brace.
      // So we hack the code extraction and
      //   back up by 2 characters

      if ((!matches_camelCase || has_underscore) &&
          !matches_operator)
      {
        addIssue(SM, range, lineIssues,
                 "Found non-camelCase function name: " + name,
                 DEFAULT_SEVERITY, -2);
      }

      // In addition to checking the name,
      // compute the cyclomatic complexity of the code.
      auto complexity = bodyComplexity(body);
      if (complexity >= COMPLEXITY_LIMIT)
      {
        addIssue(SM, range, lineIssues,
                 "(cyclomatic complexity) Function " + name + " has " +
                     std::to_string(complexity - 1) +
                     " decision points; aim for 10 or fewer per function.",
                 DEFAULT_SEVERITY, -2);
      }
      break;
    }

    case Decl::Var:
    {
      // Declaring local variables, global variables, and
      //   static data members.
      const VarDecl *f = dyn_cast<VarDecl>(decl);

      // Get the variable name
      std::string name = f->getDeclName().getAsString();

      // Check the variable name
      bool matches_camelCase = std::regex_match(name, is_camelCase);
      bool has_underscore = std::regex_match(name, final_underscore);
      bool matches_UPPER_CASE = std::regex_match(name, is_UPPER_CASE);
      bool matches_UPPER_CASE_ = std::regex_match(name, is_UPPER_CASE_);
      bool matches_internal = std::regex_match(name, is_internal);

      // Check whether the variable is constant, or could be
      auto varType = f->getType();
      bool is_const = varType.isConstQualified();
      const Expr *init = f->getAnyInitializer();

      bool has_constexpr_definition =
          isLiteral(init) ||
          // The following code crashes with assertion errors as of July 2, 2015
          //((init != nullptr) && (init->isCXX11ConstantExpr(*(Result.Context))))
          // So does this:
          //((init != nullptr) &&
          //    (init->isConstantInitializer(*(Result.Context),false))) ||
          // And this:
          //  ((init != nullptr) && (init->isIntegerConstantExpr(*(Result.Context)))) ||
          false;

      bool is_field = f->isCXXInstanceMember() || f->isStaticDataMember();

      // Diagnose
      if (!is_const &&
          !(matches_camelCase || has_underscore) &&
          !(matches_camelCase && has_underscore && f->isStaticDataMember()) &&
          !matches_internal)
      {
        addIssue(SM, range, lineIssues,
                 "Found a non-camelCase variable name: " +
                     f->getNameAsString() +

                     ((matches_UPPER_CASE && !(f->isConstexpr()) &&
                       has_constexpr_definition)
                          ? " (is this supposed to be constant?)"
                          : ""));
      }
      else if (is_const &&
               !matches_UPPER_CASE &&
               !(matches_UPPER_CASE_ && is_field) &&
               !(matches_camelCase && !has_constexpr_definition) &&
               !matches_internal)
      {
        addIssue(SM, range, lineIssues,
                 "Found a non-UPPER_CASE constant name: " +
                     f->getNameAsString());
      }
      break;
    }

    case Decl::Field:
    {
      // Get the field declaration
      const FieldDecl *f = dyn_cast<FieldDecl>(decl);
      assert(f != nullptr);

      // Check the name of the field.
      std::string name = f->getNameAsString();
      bool matches_camelCase = std::regex_match(name, is_camelCase);
      bool has_underscore = std::regex_match(name, final_underscore);
      bool matches_UPPER_CASE = std::regex_match(name, is_UPPER_CASE);

      // Check the type and initializer of the field
      auto varType = f->getType();
      bool is_const = varType.isConstQualified();
      const Expr *init = f->getInClassInitializer();
      bool has_constexpr_definition =
          isLiteral(init) || false;

      // Diagnose
      if (!matches_camelCase || !has_underscore)
      {
        addIssue(SM, range, lineIssues,
                 "Found a non-camelCase_ data member: " +
                     f->getNameAsString() +
                     (is_const && has_constexpr_definition && matches_UPPER_CASE ? " (should this be a 'static constexpr' data member?)" : ""));
      }
      break;
    }

    case Decl::Record:
    {

      // Get the struct/union/class declaration
      const CXXRecordDecl *f = dyn_cast<CXXRecordDecl>(decl);
      assert(f != nullptr);

      // To avoid duplicate name warnings, ignore forward declarations
      if (!f->isCompleteDefinition())
        break;

      // Check the name.
      std::string name = f->getNameAsString();
      bool matches_CamelCase = std::regex_match(name, is_CamelCase);
      bool has_underscore = std::regex_match(name, final_underscore);

      // Diagnose
      if (!matches_CamelCase || !has_underscore)
      {
        std::string kind = f->isClass() ? "class" : (f->isStruct() ? "struct" : "union");
        addIssue(SM, range, lineIssues,
                 "Found a non-CamelCase " + kind + " name: " +
                     f->getQualifiedNameAsString());
      }
      break;
    }

    default:
      break;
    }
  }

private:
  static bool isLiteral(const Expr *e)
  {
    if (!e)
      return false;

    // Skip any implicit or explicit casts
    if (const CastExpr *e2 = dyn_cast<CastExpr>(e))
    {
      e = e2->getSubExprAsWritten();
    }

    return isa<IntegerLiteral>(e) ||
           isa<FloatingLiteral>(e) ||
           isa<CharacterLiteral>(e) ||
           isa<clang::StringLiteral>(e);
  }
};

class ProcessMemberProj : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched projection
    const Expr *expr = Result.Nodes.getNodeAs<Expr>("expr");
    assert(expr != nullptr);

    // Get the source location of that declaration
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue(SM, range, lineIssues, "Using '->' would be better!");
  }
};

class ProcessGotoStmt : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched goto statement
    const GotoStmt *gotoStmt = Result.Nodes.getNodeAs<GotoStmt>("stmt");
    assert(gotoStmt != nullptr);

    // Get the source location of that declaration
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = gotoStmt->getSourceRange();

    addIssue(SM, range, lineIssues, "Goto considered harmful.");
  }
};

class ProcessThisProj : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched projection
    const MemberExpr *projExpr = Result.Nodes.getNodeAs<MemberExpr>("projection");
    assert(projExpr != nullptr);
    // Get the matched `this'
    const CXXThisExpr *thisExpr = Result.Nodes.getNodeAs<CXXThisExpr>("this");
    assert(thisExpr != nullptr);

    if (thisExpr->isImplicit())
    {
      // Don't yell at the user for implicit `this'!
      return;
    }

    // Get the source location of that statement
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = projExpr->getSourceRange();

    // Diagnose
    std::string fieldName = projExpr->getMemberNameInfo().getAsString();
    addIssue(SM, range, lineIssues,
             "Expression could be simplified to just '" + fieldName + "'");
  }
};

class ProcessIncDec : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched increment (or decrement) expression
    const UnaryOperator *incExpr = Result.Nodes.getNodeAs<UnaryOperator>("increment");
    assert(incExpr != nullptr);

    // Our pattern also catches prefix ++ and --; if so, nothing
    //   to complain about.
    if (incExpr->isPrefix())
      return;

    // Get the source location of that statement
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = incExpr->getSourceRange();

    // Diagnose.
    std::string operation = (incExpr->isIncrementOp() ? "increment" : "decrement");
    addIssue(SM, range, lineIssues,
             "Pre-" + operation + " is preferred to post-" + operation + " in idiomatic C++");
  }
};

// XXX: Complains about #defined constants such as RAND_MAX
class ProcessMagicNumbers : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched literal constant
    const Expr *litExpr = Result.Nodes.getNodeAs<Expr>("literal");
    assert(litExpr != nullptr);

    // See (by run-time type test) whether the expression is an integer
    //    or floating-point constant
    if (const IntegerLiteral *intExpr = Result.Nodes.getNodeAs<IntegerLiteral>("literal"))
    {

      auto value = intExpr->getValue();
      if (value == -1 || value == 0 || value == 1 || value == 2)
        // Usually not worth naming such a constant
        return;
    }
    else if (const FloatingLiteral *fltExpr =
                 Result.Nodes.getNodeAs<FloatingLiteral>("literal"))
    {

      auto value = fltExpr->getValueAsApproximateDouble();
      if (value == 0.0 || value == 1.0 || value == -1.0 || value == 2.0)
        // Usually not worth naming such a constant
        return;
    }

    // Get the source location of the literal
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = litExpr->getSourceRange();

    // Sadly, we get false positives, because named CPP macros
    // and intrinsics like sizeof can become integer literals
    // by the time this analysis runs. Double-check that we
    // are identifying numeric source code as a magic number,
    // rather than the name of some macro.

    auto bounds = getBounds(SM, range);
    SourceLocation start = bounds.first;
    SourceLocation stop = bounds.second;
    std::string code = getCode(SM, start, stop);

    if (std::regex_match(code, is_number))
    {
      addIssue(SM, range, lineIssues, "Is this a magic number?");
    }
  }
};

class ProcessEqBool : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {
    // Get the matched comparision operation (either == or !=)
    const BinaryOperator *boolExpr = Result.Nodes.getNodeAs<BinaryOperator>("expr");
    assert(boolExpr != nullptr);
    // Get the matched boolean literal
    const CXXBoolLiteralExpr *boolLit = Result.Nodes.getNodeAs<CXXBoolLiteralExpr>("bool");
    assert(boolLit != nullptr);

    bool trueConst = boolLit->getValue();
    bool equality = boolExpr->getOpcode() == BO_EQ;

    // Get the source location of that statement
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = boolExpr->getSourceRange();

    // Diagnose
    if (trueConst)
    {
      if (equality)
      {
        addIssue(SM, range, lineIssues,
                 "Redundant (or buggy!) \"== true\"");
      }
      else
      {
        addIssue(SM, range, lineIssues,
                 "The prefix ! (not) operator would be more idiomatic");
      }
    }
    else
    {
      if (equality)
      {
        addIssue(SM, range, lineIssues,
                 "The prefix ! (not) operator would be more idiomatic");
      }
      else
      {
        addIssue(SM, range, lineIssues,
                 "Redundant \"!= false\"");
      }
    }
  }
};

class ProcessNull : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched NULL or 0 expression
    const Expr *expr = Result.Nodes.getNodeAs<Expr>("expr");
    assert(expr != nullptr);

    // Get the source location of that statement
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue(SM, range, lineIssues, "Modern C++ uses nullptr");
  }
};

class ProcessCCast : public MatchFinder::MatchCallback
{
public:
  virtual void run(const MatchFinder::MatchResult &Result)
  {

    // Get the matched cast expression
    const Expr *expr = Result.Nodes.getNodeAs<Expr>("expr");
    assert(expr != nullptr);

    // Get the source location
    SourceManager &SM = *Result.SourceManager;
    SourceRange range = expr->getSourceRange();

    addIssue(SM, range, lineIssues, "Modern C++ avoids C-style casts");
  }
};

////////////////////////
//  main
///////////////////////
//
int main(int argc, const char **argv)
{
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  CommonOptionsParser OptionsParser(argc, argv, StyleCheckCategory);
  tooling::ClangTool Tool(OptionsParser.getCompilations(),
                          OptionsParser.getSourcePathList());
  MatchFinder Finder;

  //
  // ------START OF main() PATTERNS---------
  //

  //
  // Search for field accesses
  //    (*p).field_
  // and invocations
  //    (*p).method(...)
  // where p is a pointer.
  //

  ProcessMemberProj processMemberProj;

  Finder.addMatcher(
      // Look for data member accesses
      memberExpr(
          // that are not in a system header file
          unless(isExpansionInSystemHeader()),
          // ...and whose projectee
          hasObjectExpression(
              // ...has parentheses
              parenExpr(
                  has(
                      // ...containing a use of a unary operator
                      unaryOperator(
                          // ... specifically the * operator
                          hasOperatorName("*"),
                          // ... applied to an operand
                          hasUnaryOperand(
                              // ... expression
                              expr(
                                  // ... having a pointer type
                                  hasType(pointerType()),
                                  // ... but not the this keyword
                                  //  (because we handle it separately)
                                  unless(cxxThisExpr()))))))))
          .bind("expr"),
      &processMemberProj);

  Finder.addMatcher(
      // Look for member function invocations
      cxxMemberCallExpr(
          // ...that are not in a system header file
          unless(isExpansionInSystemHeader()),
          // ...and whose callee
          on(
              // ...is a unary operator
              unaryOperator(
                  // ...specifically a * operator
                  hasOperatorName("*"),
                  // ...whose operand is a pointer.
                  hasUnaryOperand(
                      expr(
                          hasType(pointerType()),
                          unless(cxxThisExpr()))))))
          .bind("expr"),
      &processMemberProj);

  //
  // Search for uses of "goto"
  //

  ProcessGotoStmt processGotoStmt;

  Finder.addMatcher(
      // Look for goto statements
      gotoStmt(
          // ...that are not in a system header file
          unless(isExpansionInSystemHeader()))
          .bind("stmt"),
      &processGotoStmt);

  //
  // Search for uses of (*this).foo or this->foo
  //
  // It seems like these same patterns also catch (*this).bar(...)
  // and this->bar(...)
  //

  ProcessThisProj processThisProj;

  Finder.addMatcher(
      // look for projections from objects
      memberExpr(
          // ...not in a system header file
          unless(isExpansionInSystemHeader()),
          // ...where that object
          hasObjectExpression(
              // ... is a parenthesized
              ignoringImpCasts(
                  parenExpr(
                      has(
                          // ... use of a unary operator,
                          unaryOperator(
                              // ... specifically, the * operator
                              hasOperatorName("*"),
                              // ...applied to
                              hasUnaryOperand(
                                  // ... the "this" object
                                  cxxThisExpr().bind("this"))))))),
          // ...and where the projection does not occur in a declaration
          // that was automatically synthesized by the compiler implicitly
          unless(hasAncestor(isImplicit())),
          // ... or synthesized because of an explicit "== default"
          unless(hasAncestor(functionDecl(isDefaulted())))
          // ...since synthesized code seems to use explicit this's.
          )
          .bind("projection"),
      &processThisProj);

  Finder.addMatcher(
      // and look for projections from
      memberExpr(
          // ... the "this" object
          hasObjectExpression(cxxThisExpr().bind("this")),
          // ... not in a system header file
          unless(isExpansionInSystemHeader()),
          // ...and where the projection does not occur in a declaration
          // that was automatically synthesized by the compiler implicitly
          unless(hasAncestor(isImplicit())),
          // ... or synthesized because of an explicit "== default"
          unless(hasAncestor(functionDecl(isDefaulted())))
          // ...since synthesized code seems to use explicit this's.
          )
          .bind("projection"),
      &processThisProj);

  //
  // Generic analysis of all declarations
  //

  ProcessDecl processDecl;

  Finder.addMatcher(
      // Look for declarations
      decl(
          // ...not in a system header file
          unless(isExpansionInSystemHeader()),
          // ...not the result of instantiating a template
          //    (because we've probably analyzed the template
          //    code itself, and we don't want duplicates)
          unless(isInstantiated()),
          // ...and not immediately inside a catch statement
          //    (where LLVM seems to generate variable names taht
          //    loook like class names)
          unless(hasParent(cxxCatchStmt())))
          .bind("decl"),
      &processDecl);

  //
  // Look for unnecessary prefix ++ and -- operations
  //
  // We catch both prefix and postfix operations in this
  // pattern, and then rely on the callback to distinguish them.
  //

  ProcessIncDec processIncDec;

  Finder.addMatcher(
      // Look unary-operator expressions,
      unaryOperator(
          // ...not in a system header file
          unless(isExpansionInSystemHeader()),
          // ...that is either ++ or --
          anyOf(hasOperatorName("++"), hasOperatorName("--")),
          // ...and
          anyOf(
              // ...either this whole thing is not immediately nested
              //    inside another expression (e.g., it's a statement
              //    by itself).
              unless(hasParent(expr())),
              // ...or the parent expression is a comma operator
              hasParent(binaryOperator(hasOperatorName(",")))))
          .bind("increment"),
      &processIncDec);

  //
  // Look for magic numbers
  //

  ProcessMagicNumbers processMagicNumbers;

  Finder.addMatcher(
      // Look for integer literals
      integerLiteral(
          // ...other than zero and one (is this really an optimization?)
          unless(equals(0)),
          unless(equals(1)),
          // ...not in system header files
          unless(isExpansionInSystemHeader()),
          // ...that are not immediately part of a declaration
          //     (presumably a definition)
          unless(hasParent(decl())),
          // ...(or of a type cast and a declaration/definition)
          unless(hasParent(implicitCastExpr(hasParent(decl())))),
          // ...and which are not part of compiler-created call to a
          //    function like __assert_rtn (used by the assert macro)
          //    because it plugs in line numbers as magic constants
          unless(hasAncestor(callExpr(callee(namedDecl(matchesName("__")))))),
          // ...(but sometime the call to __assert_rtn shows up in the AST
          //    as an unresolvedLookupExpr, so check for that.)
          ////        unless(hasAncestor(callExpr(callee(unresolvedLookupExpr(isSystemURE()))))),
          // ...and where the literal is not part of an explicit array
          //    initializer list (e.g., the constants in primeLessThan).
          unless(hasParent(initListExpr())))
          .bind("literal"),
      &processMagicNumbers);

  Finder.addMatcher(
      // Look for floating-point literals
      floatLiteral(
          // ...other than 0.0 and 1.0 (is this really an optimization?)
          unless(equals(0.0)),
          unless(equals(1.0)),
          // ...not in a system header
          unless(isExpansionInSystemHeader()),
          // ...that are not immediately part of a declaration
          //     (presumably a definition)
          unless(hasParent(decl())),
          // ...and which are not part of compiler-created call to a
          //    function like __assert_rtn (used by the assert macro)
          //    because it plugs in line numbers as magic constants
          unless(hasParent(implicitCastExpr(hasParent(decl())))),
          // ...and where the literal is not part of an explicit array
          //    initializer list (e.g., the constants in primeLessThan).
          // e.g., the constants in primeLessthan
          unless(hasParent(initListExpr())))
          .bind("literal"),
      &processMagicNumbers);

  //
  // Search for == true, != true, == false, != false
  //            true ==, true !=, false ==, false !=
  //

  ProcessEqBool processEqBool;

  Finder.addMatcher(
      // Look for binary operators
      binaryOperator(
          // ...not in a system header
          unless(isExpansionInSystemHeader()),
          // ...specifically the == and != operators
          anyOf(hasOperatorName("=="),
                hasOperatorName("!=")),
          // ...where either the left-hand-side or the
          //    right-hand-side is a boolean literal.
          anyOf(hasLHS(ignoringImpCasts(cxxBoolLiteral().bind("bool"))),
                hasRHS(ignoringImpCasts(cxxBoolLiteral().bind("bool")))))
          .bind("expr"),
      &processEqBool);

  //
  // Search for uses of NULL
  //

  ProcessNull processNull;

  /*
     // It seems like LLVM treats NULL specially, so this pattern
     // seems unnecessary
  Finder.addMatcher(
      declRefExpr(
        unless(isExpansionInSystemHeader()),
        to(varDecl(hasName("NULL")))
       ).bind("expr"),
      &processNull);
  */

  Finder.addMatcher(
      // Search for uses of the system NULL pointer
      gnuNullExpr(
          // not in a system header
          unless(isExpansionInSystemHeader()))
          .bind("expr"),
      &processNull);

  // Looking at the AST produced by clang, it seems like all
  //  *explicit* casts of 0 have *implicit* (NullToPointer) casts inside.
  // So it suffices to search for implicit type casts.
  Finder.addMatcher(
      // Search for implicit type casts
      implicitCastExpr(
          // ...of the integer 0
          has(integerLiteral(equals(0))),
          // ...not in a system header
          unless(isExpansionInSystemHeader()),
          // ...that are casting 0 to a pointer.
          hasImplicitDestinationType(pointerType()))
          .bind("expr"),
      &processNull);

  //
  // Search for uses of C-style casts
  //

  ProcessCCast processCCast;

  Finder.addMatcher(
      // Search for C-style type casts
      cStyleCastExpr(
          unless(isExpansionInSystemHeader()),
          // the assert macro seems to do the cast (void)0
          unless(has(integerLiteral(equals(0)))))
          .bind("expr"),
      &processCCast);

  //
  // ------END OF main() PATTERNS---------
  //

  // Search for all the patterns, and run all the callbacks.
  Tool.run(newFrontendActionFactory(&Finder).get());

  //Produce either terminal or HTML output
  if (HTMLoutput)
  {
    for (const auto &issue : lineIssues)
    {
      std::cout << issue.getHTML() << std::endl;
    }
  }
  else
  {
    std::cout << std::endl;
    for (const auto &issue : lineIssues)
    {
      std::cout << issue.getText() << std::endl;
    }
  }
}
