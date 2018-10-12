# include after llvm.mk

CLANG_VERSION = $(shell ls -t $(LLVM_INSTALL_PATH)/lib/clang | head -1)
CLANG_INCLUDE_DIR = $(LLVM_INSTALL_PATH)/lib/clang/$(CLANG_VERSION)/include

CPP_INCLUDE_DIR = $(shell echo | clang++ -Wp,-v -stdlib=libc++ -x c++ - -fsyntax-only 2>&1 | egrep "^ " | head -1)

#$(info CPP_INCLUDE_DIR $(CPP_INCLUDE_DIR))
#CPP_INCLUDE_DIR = /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/

# These go *before* LLVM's libraries in the command line
CLANG_LIBS = \
        -lclangTooling \
        -lclangFrontendTool \
        -lclangFrontend \
        -lclangDriver \
        -lclangToolingCore \
        -lclangSerialization \
        -lclangCodeGen \
        -lclangParse \
        -lclangSema \
        -lclangStaticAnalyzerFrontend \
        -lclangStaticAnalyzerCheckers \
        -lclangStaticAnalyzerCore \
        -lclangAnalysis \
        -lclangARCMigrate \
        -lclangRewrite \
        -lclangRewriteFrontend \
        -lclangEdit \
        -lclangAST \
        -lclangASTMatchers \
        -lclangLex \
        -lclangBasic \
        -lclang


CLANG_RUNTIME_INCLUDES = -isystem$(CLANG_INCLUDE_DIR) -isystem$(CPP_INCLUDE_DIR)


