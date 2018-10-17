# include after llvm.mk

include $(CSTOOLS_ROOT)/clang.$(shell hostname -s).mk


#CPP_INCLUDE_DIR = $(shell echo | /usr/bin/clang++ -Wp,-v -x c++ -stdlib=libc++ -fsyntax-only - 2>&1 | egrep "^ " | head -1)
CPP_INCLUDE_DIR = $(shell echo | clang++ -Wp,-v -x c++ -stdlib=libc++ -fsyntax-only - 2>&1 | egrep "^ " | grep "v1" | head -1)

$(info CPP_INCLUDE_DIR $(CPP_INCLUDE_DIR))
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

$(info CLANG_INCLUDE_DIR $(CLANG_INCLUDE_DIR))
CLANG_RUNTIME_INCLUDES = -isystem$(CPP_INCLUDE_DIR) -isystem$(CLANG_INCLUDE_DIR) -isystem/usr/include


