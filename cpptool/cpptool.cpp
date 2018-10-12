#include <string>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

static unordered_set<string> potentialReplacements;
static unordered_map<string,string> replacements;
static bool replaceAll = false;


/////////////////////////
// COMMAND-LINE OPTIONS 
/////////////////////////

static cl::OptionCategory ReplaceToolCategory("Replacer Options");
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
//static cl::extrahelp MoreHelp("\nMore help text...");

static cl::list<string> functionsToReplace(
      "f",
      cl::desc("function to replace"),
      cl::ZeroOrMore,
      cl::cat(ReplaceToolCategory));

static cl::list<string> replacementFiles(
      "r",
      cl::desc("replacement file"),
      cl::ZeroOrMore,
      cl::cat(ReplaceToolCategory));

static cl::opt<bool> dumpFunctions(
      "d",
      cl::desc("Dump the functions defined in the specified files"),
      cl::cat(ReplaceToolCategory));

static cl::opt<bool> canonicalTypes(
      "c",
      cl::desc("Print canonicalized types"),
      cl::cat(ReplaceToolCategory));

const char * const ADDITIONAL_HELP = "Replaces designated functions or member functions in C++ source";

//////////////////////
// HELPER FUNCTIONS
//////////////////////

std::string nameOfType(PrintingPolicy Policy, QualType ty)
{
  if (canonicalTypes) ty = ty.getCanonicalType();
  return ty.getAsString(Policy);
}

// Converts the AST for a function declaration into 
//   a human-readable (i.e., demangled) string representation.
// 
std::string nameOfDecl(PrintingPolicy Policy, FunctionDecl* f, bool showReturnTy = false)
{
  std::string funcName = f->getQualifiedNameAsString();
  std::string fullName = funcName + "(";
  ArrayRef<ParmVarDecl*> parameters = f->parameters();

  for (const auto& pvd : parameters) {
    if (&pvd != &parameters[0]) fullName += ", ";
    fullName += nameOfType(Policy, pvd->getType());
  }
  fullName += ")";

  if (CXXMethodDecl* m = dyn_cast<CXXMethodDecl>(f)) {
    if (m->isConst()) {
      fullName += " const";
                }
  }

  if (showReturnTy) {
     QualType returnTy = f->getReturnType();
//     fullName = returnTy.getCanonicalType().getAsString(Policy) + " " + fullName;
     fullName = nameOfType(Policy, returnTy) + " " + fullName;
  }

  return fullName;
}

///////////////////////////////////
// Code for Extracting Functions //
///////////////////////////////////

// ASTConsumer is an interface used to write generic actions on an AST,
// regardless of how the AST was produced. 
// s̄ASTConsumer provides many different entry points

using action_t = std::function<void(const SourceManager&, const LangOptions&, Decl*)>;

class MyASTSearchConsumer : public ASTConsumer {
public:
//  MyASTSearchConsumer(Rewriter &R) : TheRewriter{R} {}
  MyASTSearchConsumer(const ASTContext& context, const action_t& action) : 
      sm_{context.getSourceManager()}, lo_{context.getLangOpts()}, action_{action} 
  {
     // Nothing (else) to do!
  }

  virtual void HandleInlineFunctionDefinition(FunctionDecl* d) override {
    action_(sm_, lo_, d);
  }
  // Override the method that gets called for each parsed top-level
  // declaration.
  virtual bool HandleTopLevelDecl(DeclGroupRef DR) override {

    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
      action_(sm_, lo_, *b);
      /*
      if (FunctionDecl* f = dyn_cast<FunctionDecl>(*b)) {
        if (f->hasBody()) {
          string fullName = nameOfDecl(LangOpts, f);

          auto i = potentialReplacements.find(fullName);

          if (replaceAll || i != potentialReplacements.end()) {

            SourceRange range = f->getSourceRange();
            SourceLocation start = range.getBegin();
            SourceLocation stop = range.getEnd();

            if (start.isMacroID()) break;
            if (SM.isInSystemHeader(start)) break;
            llvm::errs() << "Search found the extracted function " << fullName << "\n";

            //llvm::errs() << "At location" <<
              //TheRewriter.getSourceMgr().getFilename(start) << "\n";

            // Clang source ranges are closed intervals, meaning
            // that 'stop' identifies the last token in the function,
            // i.e., the closing right curly brace. We could just
            // add 1 character to get the open interval needed below,
            // but let's be good and programmatically compute the
            // length of this final token.
            size_t offset = Lexer::MeasureTokenLength(stop, SM, LangOpts);

            std::string code =
              std::string(SM.getCharacterData(start),
                  SM.getCharacterData(stop)-SM.getCharacterData(start)+offset);

            replacements.insert(make_pair(fullName, code));
          }
        }
      }
      */
    }
    return true;
  }

private:
//  Rewriter& TheRewriter;
//    ASTContext& context_;
    const SourceManager& sm_;
    const LangOptions& lo_;
    action_t action_;
};


void listUserFunctions(const SourceManager& SM, const LangOptions& LangOpts, Decl* decl)
{
     if (FunctionDecl* f = dyn_cast<FunctionDecl>(decl)) {
//        if (f->isThisDeclarationADefinition()) {
//        if (f->hasBody()) {
          string fullName = nameOfDecl(LangOpts, f, true);

            SourceRange range = f->getSourceRange();
            SourceLocation start = range.getBegin();
//            SourceLocation stop = range.getEnd();

            if (start.isMacroID()) return;
            //if (SM.isInSystemHeader(start)) return;
            if (! SM.isInMainFile(start)) return;
            llvm::outs() << fullName << "\n";
//            llvm::errs() << "At location" << SM.getFilename(start) << "-" 
//                         << SM.getFilename(stop) << "\n";
//        }
     }
}

class MySearchAction : public ASTFrontendAction {
public:
  MySearchAction() {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override
  {
    // llvm::errs() << "** Creating AST consumer for: " << file << "\n";
    // TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTSearchConsumer>(CI.getASTContext(), listUserFunctions); // was (TheRewriter);
  }

private:
//  Rewriter TheRewriter;
};

#if 0

///////////////////////////////
// Code for Replacing Functions
///////////////////////////////


// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser.
class MyASTReplaceConsumer : public ASTConsumer {
public:
  MyASTReplaceConsumer(Rewriter &R, Replacements& P) :
    TheRewriter{R}, TheReplacements{P} {}

  // Override the method that gets called for each parsed top-level
  // declaration.
  virtual bool HandleTopLevelDecl(DeclGroupRef DR) {

    const SourceManager& SM = TheRewriter.getSourceMgr();
    const LangOptions& LangOpts = TheRewriter.getLangOpts();

    //llvm::errs() << "Found a group\n";
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
      if (FunctionDecl* f = dyn_cast<FunctionDecl>(*b)) {
        if (f->hasBody()) {

          //llvm::errs() << "Saw function " << funcName << "\n";
          //SourceRange range = f->getSourceRange();
          //SourceLocation start = range.getBegin();
          //llvm::errs() << "At location" <<
          //TheRewriter.getSourceMgr().getFilename(start) << "\n";

          //ArrayRef<ParmVarDecl*> parameters = f->parameters();

          string fullName = nameOfDecl(LangOpts, f);
          auto i = replacements.find(fullName);
          if (i != replacements.end()) {
            SourceRange range = f->getSourceRange();
            Replacement repl{SM, CharSourceRange{range,true}, i->second};
            auto err = TheReplacements.add(repl);
            if (err) {
              cerr << "Replacement error" << endl;
              //cerr << err << endl;
              exit(-42);
            }
            // TheRewriter.ReplaceText(f->getSourceRange(), i->second);
          }
        }
      }
    }
    return true;
  }

private:
  Rewriter &TheRewriter;
  Replacements& TheReplacements;
};

// HACK
std::map< std::string, Replacements >  repl;

// For each source file provided to the tool, a new FrontendAction is created.

class MyReplaceAction : public ASTFrontendAction {
public:
  MyReplaceAction() {}

  void EndSourceFileAction() override
  {
    //SourceManager &SM = TheRewriter.getSourceMgr();

    //llvm::errs() << "** EndSourceFileAction for: "
                 //<< SM.getFileEntryForID(SM.getMainFileID())->getName() << "\n";

    // Now emit the rewritten buffer.
    //TheRewriter.getEditBuffer(SM.getMainFileID()).write(llvm::outs());
  }


  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override
  {
    //llvm::errs() << "** Creating AST consumer for: " << file << "\n";

    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTReplaceConsumer>(TheRewriter,
                                                   repl);
  }

private:
  Rewriter TheRewriter;
};

///////////////////////////////////////////////////////////////////////

#endif  

int main(int argc, const char **argv) {
  // llvm::sys::PrintStackTraceOnErrorSignal();

  CommonOptionsParser OptionsParser(argc, argv, ReplaceToolCategory, ADDITIONAL_HELP);

  if (dumpFunctions) {
  ClangTool SearchTool(OptionsParser.getCompilations(),
                        OptionsParser.getSourcePathList());
  int searchError = SearchTool.run(newFrontendActionFactory<MySearchAction>().get());
  }
#if 0
  for (auto& s : functionsToReplace) {
    potentialReplacements.insert(s);
  }

  ClangTool SearchTool(OptionsParser.getCompilations(),
                        vector<string>(replacementFiles) );

  if (functionsToReplace.size() == 0) {
    replaceAll = true;
  }

  int searchError = SearchTool.run(newFrontendActionFactory<MySearchAction>().get());

  if (searchError) {
    llvm::errs() << "**Problem reading the file of replacements:";
    for (auto& filename : replacementFiles) {
       llvm::errs() << " " << filename;
    }
    llvm::errs() << "\n";
    exit(searchError);
  }
#endif

#if 0
  RefactoringTool ReplaceTool(OptionsParser.getCompilations(),
                        OptionsParser.getSourcePathList());
  //HACK
  repl = &ReplaceTool.getReplacements();
  return ReplaceTool.runAndSave(newFrontendActionFactory<MyReplaceAction>().get());

/*
  // At this point the rewriter's buffer should be full with the rewritten
  // file contents.
  const RewriteBuffer *RewriteBuf =
      TheRewriter.getRewriteBufferFor(SourceMgr.getMainFileID());
  llvm::outs() << std::string(RewriteBuf->begin(), RewriteBuf->end());
  */
#endif
  return 0;

}
