// e.g., 
//    ./run -e=foo.expect foo.cpp
//    (cd testing; ../run -extract="IntList::push_front" cs70-intlist-good.cpp)

#include <algorithm>
#include <string>
#include <fstream>
#include <iostream>
#include <iterator>
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

//static unordered_set<string> potentialReplacements;
//static unordered_map<string,string> replacements;
//static bool replaceAll = false;

//static unordered_map<string,string> componentAccess;

static std::vector<std::string> expectedMembers;
static std::unordered_set<std::string> foundMembers;

static std::unordered_map<std::string, std::string> declarations;
static std::unordered_map<std::string, std::string> definitions;


/////////////////////////
// COMMAND-LINE OPTIONS 
/////////////////////////

static cl::OptionCategory ReplaceToolCategory("Replacer Options");
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
//static cl::extrahelp MoreHelp("\nMore help text...");

/*
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
*/

static cl::opt<string> definitionToExtract(
      "extract",
      cl::desc("member to extract"),
      cl::cat(ReplaceToolCategory));

static cl::opt<string> expectedMemberFile(
      "e",
      cl::desc("Specify expected members"),
      cl::value_desc("filename"),
      cl::cat(ReplaceToolCategory));

static cl::opt<bool> dumpMembers(
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

std::string nameOfAccess(AccessSpecifier access)
{
  switch (access) {
    case AS_public: return "public "; 
    case AS_private: return "private ";
    case AS_protected: return "protected ";
    default: return "";
  }
}

// Converts the AST for a function declaration into 
//   a human-readable (i.e., demangled) string representation.
// 
std::string nameOfDecl(PrintingPolicy Policy, NamedDecl* nd, 
                       bool showReturnTy = false, bool showAccess = false)
{
  std::string fullName = nd->getQualifiedNameAsString();

  if (FunctionDecl* f = dyn_cast<FunctionDecl>(nd)) {
    fullName +="(";
    ArrayRef<ParmVarDecl*> parameters = f->parameters();


    for (const auto& pvd : parameters) {
      if (&pvd != &parameters[0]) fullName += ", ";
      fullName += nameOfType(Policy, pvd->getType());
    }
    fullName += ")";


    if (showReturnTy) {
      QualType returnTy = f->getReturnType();
      fullName = nameOfType(Policy, returnTy) + " " + fullName;
    }

    if (CXXMethodDecl* m = dyn_cast<CXXMethodDecl>(nd)) {
      if (m->isConst()) {
        fullName += " const";
      }

      if (m->isDeleted()) {
        fullName += " = delete";
      } else if (m->isDefaulted()) {
        fullName += " = default";
      }

      if (m->isVirtual()) {
        fullName = "virtual " + fullName;
      }
    }

    if (showAccess) {
      fullName = nameOfAccess(f->getCanonicalDecl()->getAccess()) + fullName;
    }
  } else if (FieldDecl* f = dyn_cast<FieldDecl>(nd)) {
      fullName = nameOfType(Policy, f->getType()) + " " + fullName;
      fullName = nameOfAccess(f->getAccess()) + fullName;
  }

  return fullName;
}

///////////////////////////////////
// Code for Extracting Functions //
///////////////////////////////////

// ASTConsumer is an interface used to write generic actions on an AST,
// regardless of how the AST was produced. 
// ASTConsumer provides many different entry points

// using action_t = std::function<void(const SourceManager&, const LangOptions&, Decl*)>;

class MyASTSearchVisitor
  : public RecursiveASTVisitor<MyASTSearchVisitor> {
public:
  MyASTSearchVisitor(const SourceManager& sm, const LangOptions& lo) 
    : sm_{sm}, lo_{lo}
  {
      // Nothing (else) to do
  }

  bool VisitDecl(Decl* decl) {
    // For debugging, dumping the AST nodes will show which nodes are already
    // being visited.
    //Declaration->dump();

   SourceRange range = decl->getSourceRange();
   SourceLocation start = range.getBegin();
   SourceLocation stop = range.getEnd();
   if (sm_.isInSystemHeader(start)) return true;
   if (start.isMacroID()) return true;


   if (FunctionDecl* f = dyn_cast<FunctionDecl>(decl)) {


      if (f->isThisDeclarationADefinition()) {
      // if (f->hasBody()) {

            string fullName = nameOfDecl(lo_, f, true, true);
            std::string memberDescription = "define " + fullName;
            foundMembers.insert(memberDescription);
            if (dumpMembers) llvm::outs() << memberDescription << "\n";

            if (definitionToExtract != "" && fullName.find(definitionToExtract) != string::npos) {
                
                // https://stackoverflow.com/questions/25275212/how-to-extract-comments-and-match-to-declaration-with-recursiveastvisitor-in-lib
                if (const RawComment* rc = f->getASTContext().getRawCommentForDeclNoCache(f)) {
                  start = rc->getBeginLoc();
                }

                // Clang source ranges are closed intervals, meaning
                // that 'stop' identifies the last token in the function,
                // i.e., the closing right curly brace. We could just
                // add 1 character to get the open interval needed below,
                // but let's be good and programmatically compute the
                // length of this final token.
                size_t offset = Lexer::MeasureTokenLength(stop, sm_, lo_);

                std::string code =
                  std::string(sm_.getCharacterData(start),
                      sm_.getCharacterData(stop)-sm_.getCharacterData(start)+offset);
                llvm::outs() << code << "\n";            
            }
//            llvm::errs() << "At location " << SM.getFilename(start) << "-" 
//                         << SM.getFilename(stop) << "\n";
       } else {
            string fullName = nameOfDecl(lo_, f, true, true);
            std::string memberDescription = "declare " + fullName;
            foundMembers.insert(memberDescription);
            if (dumpMembers) llvm::outs() << memberDescription << "\n";
            //            llvm::outs() << "At location " << sm_.getFilename(start) << "-" 
//                         << sm_.getFilename(stop) << "\n";

       }
      
   } else if (FieldDecl* f = dyn_cast<FieldDecl>(decl)) {
            string fullName = nameOfDecl(lo_, f, true, true);
            std::string memberDescription = "field " + fullName;
            foundMembers.insert(memberDescription);
            if (dumpMembers) llvm::outs() << memberDescription << "\n";

   }

   // The return value indicates whether we want the visitation to proceed.
   // Return false to stop the traversal of the AST.
   return true;
  }

  bool VisitStmt(Stmt *st) {
        if (ReturnStmt *ret = dyn_cast(st)) {
            rewriter.ReplaceText(ret->getRetValue()->getLocStart(), 6, "val");
            errs() << "** Rewrote ReturnStmt\n";
        }
        if (CallExpr *call = dyn_cast(st)) {
            rewriter.ReplaceText(call->getLocStart(), 7, "add5");
            errs() << "** Rewrote function call\n";
        }
        return true;
    }
private:
    const SourceManager& sm_;
    const LangOptions& lo_;
};


class MyASTSearchConsumer : public ASTConsumer {
public:
//  MyASTSearchConsumer(Rewriter &R) : TheRewriter{R} {}
  MyASTSearchConsumer(const ASTContext& context) : 
      visitor_{context.getSourceManager(), context.getLangOpts()}
  {
     // Nothing (else) to do!
  }

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    // Traversing the translation unit decl via a RecursiveASTVisitor
    // will visit all nodes in the AST.
    visitor_.TraverseDecl(Context.getTranslationUnitDecl());
  }
  
private:
    MyASTSearchVisitor visitor_;q
};

/*
void listUserFunctions(const SourceManager& SM, const LangOptions& LangOpts, Decl* decl)
{
   SourceRange range = decl->getSourceRange();
   SourceLocation start = range.getBegin();
   SourceLocation stop = range.getEnd();
   if (SM.isInSystemHeader(start)) return;

   if (CXXMethodDecl* f = dyn_cast<CXXMethodDecl>(decl)) {

      // if (start.isMacroID()) return;

//      if (f->isThisDeclarationADefinition()) {
      // if (f->hasBody()) {

//            string fullName = nameOfDecl(LangOpts, f, true);
//            llvm::outs() << fullName << "\n";
//            llvm::errs() << "At location " << SM.getFilename(start) << "-" 
//                         << SM.getFilename(stop) << "\n";
 //      } else {
            string fullName = nameOfDecl(LangOpts, f, true, true);
            llvm::outs() << "declare " << fullName << "\n";
            llvm::outs() << "At location " << SM.getFilename(start) << "-" 
                         << SM.getFilename(stop) << "\n";

//       }
   }
}
*/

class MySearchAction : public ASTFrontendAction {
public:
  MySearchAction() {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override
  {
    // llvm::errs() << "** Creating AST consumer for: " << file << "\n";
    // TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTSearchConsumer>(CI.getASTContext()); // was (TheRewriter);
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

  bool checkExpectedMembers = expectedMemberFile != "";

  if (checkExpectedMembers) {
    std::ifstream in(expectedMemberFile);
    if (! in.good()) {
      llvm::errs() << "Can't open expectation file " << expectedMemberFile << "\n";
      exit(-1);
    }  
    std::string line;
    while (std::getline(in, line))
    {
       expectedMembers.push_back(line);
    }

  }

  ClangTool SearchTool(OptionsParser.getCompilations(),
                       OptionsParser.getSourcePathList());
  int searchError = SearchTool.run(newFrontendActionFactory<MySearchAction>().get());
  if (searchError) {
       llvm::errs() << "**Problem searching for function names**\n";
  }
  
  if (checkExpectedMembers) {
    for (const std::string& s : expectedMembers) {
      if (foundMembers.find(s) == foundMembers.end()) {
        llvm::errs() << "MISSING: " << s << "\n";
      }
    }
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

