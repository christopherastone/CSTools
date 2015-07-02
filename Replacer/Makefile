include ../llvm.mk
include ../clang.mk

CXX = clang++    # Use the system compiler for speed

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 -stdlib=libc++ $(LLVM_CXXFLAGS)
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS)



# the LLVM_CONFIG dependency is just there to double-check that
# we have our path set correctly
all: $(LLVM_CONFIG) replacer

replacer: replacer.o 
	@echo Linking $@
	$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

replacer.o: replacer.cpp 
	@echo Compiling $*.cpp
	$(CXX) -c $(CXXFLAGS) $<
	#$(CXX) $(CXXFLAGS) -c $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) $< -o $@

clean:
	rm -f replacer replacer.o

test: all
	(cd testing; $(MAKE) test2)

