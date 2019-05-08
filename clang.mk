# include after llvm.mk

include $(CSTOOLS_ROOT)/clang.$(shell hostname -s).mk



$(info CPP_INCLUDE_DIR $(CPP_INCLUDE_DIR))

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
#CLANG_RUNTIME_INCLUDES = -I$(CPP_INCLUDE_DIR) -I$(CLANG_INCLUDE_DIR) -isystem/usr/include -isystem/usr/include/c++/v1
CLANG_RUNTIME_INCLUDES = -I$(CPP_INCLUDE_DIR) -I$(CLANG_INCLUDE_DIR) 


