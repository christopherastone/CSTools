#include "clang/AST/ASTConsumer.h"
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

#include "issue.hpp"

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


std::vector<Issue> lineIssues;


std::regex is_camelCase = std::regex("(.*::)?[a-z][a-z0-9_]*([A-Z][a-zA-Z0-9_]*)*");
std::regex is_CamelCase = std::regex("(.*::)?[A-Z][a-z0-9_]*([A-Z][a-zA-Z0-9_]*)*");
std::regex final_underscore = std::regex(".*_");
std::regex is_operator = std::regex("(.*::)?(operator[^A-Za-z]+|operator .*)");
std::regex is_UPPER_CASE = std::regex("(.*::)?[A-Z][A-Z0-9]*(_[A-Z0-9]+)*");

std::regex is_headerFile = std::regex(".*\\.h(pp)?");

///////////////////////////////
// Callbacks for Nodes Found
///////////////////////////////

// Each class defines a run() method handling matches
//    for a different rule.
// For simplicity, we declare and define all the code
//    of the class in one place.


class ProcessDecl : public MatchFinder::MatchCallback {
public:
  ProcessDecl(Replacements* Replace) : Replace(Replace) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const Decl *decl = Result.Nodes.getNodeAs<Decl>("decl");

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceRange range = decl->getSourceRange();
    SourceLocation start = range.getBegin();
    SourceLocation stop = range.getEnd();

    size_t offset = Lexer::MeasureTokenLength(stop, SM, LangOptions());
    std::string code;

    if (SM.isMacroArgExpansion(start)) {
      start = SM.getFileLoc(start);
    }

    if (SM.isMacroArgExpansion(stop)) {
      stop = SM.getFileLoc(stop);
    }

    // Wasteful for things like namespace declarations, whose contents might be an entire C++
    // file.
    if (SM.isWrittenInSameFile(start, stop)) {
      ptrdiff_t len = SM.getCharacterData(stop)-SM.getCharacterData(start)+offset;
      assert(len >= 0);
      assert(len < 10000000);
      code = std::string(SM.getCharacterData(start), len);
    } else {
      code = "...code not available...";
    }

    switch (decl->getKind()) {
      case Decl::UsingDirective:
        {
          std::string filename = SM.getFilename(start);
          if (std::regex_match(filename, is_headerFile)) {
            lineIssues.emplace_back(
              filename,
              SM.getPresumedLineNumber(start),
              SM.getPresumedColumnNumber(start),
              "Found 'using namespace' in a header",
              code,
              Severity::WARNING
            );
          }
          break;
        }
/*
      case Decl::FieldDecl:

        lineIssues.emplace_back(
          SM.getFilename(start),
          SM.getPresumedLineNumber(start),
          SM.getPresumedColumnNumber(start),
          "Found a " + std::string(decl->getDeclKindName()) + "declaration",
          code,
          Severity::WARNING
        );
        break;
*/

      case Decl::Function:
      case Decl::CXXMethod:
        break;
        {
          const FunctionDecl* f = dyn_cast<FunctionDecl>(decl);
          if (! f->doesThisDeclarationHaveABody()) break;

          std::string name = f->getDeclName().getAsString();
          bool matches_camelCase = std::regex_match(name, is_camelCase);
          bool matches_operator = std::regex_match(name, is_operator);
          bool has_underscore = std::regex_match(name, final_underscore);
          if ( (! matches_camelCase || has_underscore) &&
               ! matches_operator) {
            cout << "'" << name << "' " << matches_operator << endl;
            lineIssues.emplace_back(
              SM.getFilename(start),
              SM.getPresumedLineNumber(start),
              SM.getPresumedColumnNumber(start),
              "Found non-camelCase function name" + f->getQualifiedNameAsString(),
              code,
              Severity::WARNING
            );
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
          auto varType = f->getType();
          bool is_const = varType.isConstQualified();
          const Expr* init = f->getAnyInitializer();
          bool has_constexpr_definition =
            (f->isConstexpr()) ||
            ((init != nullptr) && (init->isCXX11ConstantExpr(*(Result.Context))));

          if (!is_const &&
              ! (matches_camelCase || has_underscore) &&
              ! (matches_camelCase && has_underscore && f->isStaticDataMember())) {
            lineIssues.emplace_back(
              SM.getFilename(start),
              SM.getPresumedLineNumber(start),
              SM.getPresumedColumnNumber(start),
              "Found a non-camelCase variable name" + f->getQualifiedNameAsString() +
                ((matches_UPPER_CASE && has_constexpr_definition) ? " (is this supposed to be constant?)" : ""),
              code,
              Severity::ERROR
            );
          } else if (is_const &&
                      ! matches_UPPER_CASE &&
                      ! (matches_camelCase && !has_constexpr_definition)) {
              //init->dump();
              lineIssues.emplace_back(
                SM.getFilename(start),
                SM.getPresumedLineNumber(start),
                SM.getPresumedColumnNumber(start),
                "Found a non-UPPER_CASE constant name" + f->getQualifiedNameAsString() +
                  to_string(matches_UPPER_CASE) + to_string(matches_camelCase) +
                  to_string(has_constexpr_definition),
                code,
                Severity::ERROR
              );
          }
          break;
        }

      case Decl::Field:
        {
          const FieldDecl* f = dyn_cast<FieldDecl>(decl);
          std::string name = f->getNameAsString();
          bool matches_camelCase = std::regex_match(name, is_camelCase);
          bool has_underscore = std::regex_match(name, final_underscore);

          if (!matches_camelCase || !has_underscore) {
            lineIssues.emplace_back(
              SM.getFilename(start),
              SM.getPresumedLineNumber(start),
              SM.getPresumedColumnNumber(start),
              "Found a non-camelCase_ field name" + f->getQualifiedNameAsString(),
              code,
              Severity::ERROR
            );
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
            lineIssues.emplace_back(
              SM.getFilename(start),
              SM.getPresumedLineNumber(start),
              SM.getPresumedColumnNumber(start),
              "Found a non-CamelCase " + kind + " name: " + f->getQualifiedNameAsString(),
              code,
              Severity::ERROR
            );
          }
          break;
        }

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

private:
  Replacements* Replace;
};


class ProcessLowerCaseClassName : public MatchFinder::MatchCallback {
public:
  ProcessLowerCaseClassName(Replacements* Replace) : Replace(Replace) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const CXXRecordDecl *decl = Result.Nodes.getNodeAs<CXXRecordDecl>("class");

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceLocation start = decl->getLocStart();

    std::cout << "Found class name "
              << decl->getNameAsString()
              << " with lowercase first letter at "
              << start.printToString(SM)
        << std::endl;
  }

private:
  Replacements* Replace;
};


class ProcessMemberCall : public MatchFinder::MatchCallback {
public:
  ProcessMemberCall(Replacements* Replace) : Replace(Replace) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const CXXMemberCallExpr *expr = Result.Nodes.getNodeAs<CXXMemberCallExpr>("callWithPointer");

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceLocation start = expr->getLocStart();

    std::cout << "Found call using (*foo).bar(...) notation instead of foo->bar(...) notation at "
              << start.printToString(SM)
        << std::endl;
  }

private:
  Replacements* Replace;
};


class ProcessGotoStmt : public MatchFinder::MatchCallback {
public:
  ProcessGotoStmt(Replacements* Replace) : Replace(Replace) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched class-declaration AST node
    const GotoStmt *decl = Result.Nodes.getNodeAs<GotoStmt>("goto");

    // Get the source location of that declaration
    SourceManager& SM = *Result.SourceManager;
    SourceLocation start = decl->getLocStart();

    std::cout << "Go To Statement Considered Harmful. "
              << start.printToString(SM)
        << std::endl;
  }

private:
  Replacements* Replace;
};

class ProcessThisCall : public MatchFinder::MatchCallback {
public:
  ProcessThisCall(Replacements* Replace) : Replace(Replace) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    // Get the matched `this' AST node, NOT the whole this->foo expression!
    const CXXThisExpr* stmt = Result.Nodes.getNodeAs<CXXThisExpr>("myThis");
    if (stmt->isImplicit()) {
        // Don't yell at the user for implicit `this'!
        return;
    }

    if (0) {
        /* This is just here to trigger the this->f() test */
        this->run(Result);
        /* ... and this is here to not trigger it! */
        run(Result);
    }

    // Get the source location of that statement
    SourceManager& SM = *Result.SourceManager;
    SourceLocation start = stmt->getLocStart();

    std::cerr << "Found an explicit use of this->foo() instead of foo() at "
              << start.printToString(SM)
              << std::endl;
  }

private:
  Replacements* Replace;
};

////////////////////////
//  main
///////////////////////
//
int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  CommonOptionsParser OptionsParser(argc, argv, StyleCheckCategory);
  tooling::RefactoringTool Tool(OptionsParser.getCompilations(),
                                OptionsParser.getSourcePathList());
  MatchFinder Finder;

  // Create callback object(s) for each rule.
/*  ProcessNamespaceInHeader cb1(&Tool.getReplacements());
  ProcessDataMemberWithoutUnderscore cb2(&Tool.getReplacements());
  ProcessVariableWithUnderscore cb3(&Tool.getReplacements());
  ProcessLowerCaseClassName cb4(&Tool.getReplacements());
  ProcessMemberCall cb5(&Tool.getReplacements());
  ProcessGotoStmt cb6(&Tool.getReplacements());
  ProcessThisCall  cb7(&Tool.getReplacements());
*/
  ProcessDecl  cbDecl(&Tool.getReplacements());

/*
  // Add the rules, and the associated callbacks
  Finder.addMatcher(
      usingDirectiveDecl(unless(isExpansionInSystemHeader()),
                         isExpansionInFileMatching("\\.hpp$")).bind("namespace"),
      &cb1);


  Finder.addMatcher(
      fieldDecl(unless(isExpansionInSystemHeader()),
                matchesName("[^_]$")                 ).bind("dataMember"),
      &cb2);

  // This matcher matches all variable declarations. We filter out the non-local ones in
  // ProcessVariableWithUnderscore's run() method.
  Finder.addMatcher(
      varDecl(unless(isExpansionInSystemHeader()),
              matchesName("_$")                    ).bind("variable"),
      &cb3);

  Finder.addMatcher(
      recordDecl(unless(isExpansionInSystemHeader()),
                 matchesName("::[^A-Z]")              ).bind("class"),
      &cb4);

  Finder.addMatcher(
      memberCallExpr(unless(isExpansionInSystemHeader()),
                     on(unaryOperator(hasOperatorName("*")))).bind("callWithPointer"),
      &cb5);

  Finder.addMatcher(
      gotoStmt(unless(isExpansionInSystemHeader())).bind("goto"),
      &cb6);

  Finder.addMatcher(
      memberCallExpr(
          on(thisExpr().bind("myThis")),
          unless(isExpansionInSystemHeader())),
      &cb7);
*/
  Finder.addMatcher(
      decl(unless(isExpansionInSystemHeader())).bind("decl"),
      &cbDecl);

  // Run everything. If we suggested doing any replacements,
  // save the changes to disk
  Tool.run(newFrontendActionFactory(&Finder).get());

  //Sort the issues by line number
  std::sort(lineIssues.begin(), lineIssues.end());

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

/*

#include <iostream>
#include <fstream>
#include <streambuf>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>

#include "issue.hpp"
#include "declcheck.hpp"

using namespace std;

const string USAGE_MESSAGE = "usage:\nstylecheck [--html] filename";


int main(int argc, char** argv) {

  //We need at least 2 arguments to work
  if (argc < 2) {
    cout << USAGE_MESSAGE << endl;
    return 1;
  }

  //Parse the arguments
  int i = 0;
  for(; i < argc-1; ++i) {
    if (strcmp(argv[i], "--html") == 0) {
      HTMLoutput = true;
    }
  }

  //Get the filename
  string filename{argv[i]};







  return 0;
}
*/

