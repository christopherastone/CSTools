LLVM_BIN_PATH := $$HOME/llvm-install/bin

$(info -----------------------------------------------)
$(info Using LLVM_BIN_PATH = $(LLVM_BIN_PATH))
$(info -----------------------------------------------)

LLVM_CONFIG := $(LLVM_BIN_PATH)/llvm-config
CXX := clang++

CPPFLAGS += $(shell $(LLVM_CONFIG) --cppflags)
CXXFLAGS += -std=c++11 $(shell $(LLVM_CONFIG) --cxxflags)
LDFLAGS += $(shell $(LLVM_CONFIG) --ldflags) 


LIBS += \
	-lclangFrontendTool \
	-lclangFrontend \
	-lclangDriver \
	-lclangTooling \
	-lclangSerialization \
	-lclangCodeGen \
	-lclangParse \
	-lclangSema \
	-lclangRewriteFrontend \
	-lclangRewrite \
	-lclangStaticAnalyzerFrontend \
	-lclangStaticAnalyzerCheckers \
	-lclangStaticAnalyzerCore \
	-lclangARCMigrate \
	-lclangAnalysis \
	-lclangEdit \
	-lclangAST \
	-lclangLex \
	-lclangBasic

LIBS += $(shell $(LLVM_CONFIG) --libs --system-libs)

#LIBS += -lncurses -lz

all: replacer

replacer: replacer.o 
	@echo Linking $@
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS)

replacer.o: replacer.cpp 
	@echo Compiling $*.cpp
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $<
	#$(CXX) $(CXXFLAGS) -c $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) $< -o $@

clean:
	rm -f replacer replacer.o

test: all
	(cd testing; $(MAKE) test)

