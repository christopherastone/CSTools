include ../llvm.mk
include ../clang.mk

CXX = $(LLVM_BIN_PATH)/clang++    
##CXX = clang++    

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 $(LLVM_CXXFLAGS) $(CLANG_RUNTIME_INCLUDES)

LDFLAGS  += $(LLVM_LDFLAGS)
BOOST_LIBS = -lboost_filesystem -lboost_system

LIBS += $(CLANG_LIBS) $(LLVM_LIBS) $(BOOST_LIBS)

CLANGCOMMAND = clang++ $(CLANG_RUNTIME_INCLUDES) -std=c++11 -c

TARGETS = stylechecker stylecheck

# the LLVM_CONFIG dependency is just there to double-check that
# we have our path set correctly
all: $(LLVM_CONFIG) $(TARGETS)

stylechecker: stylechecker.o issue.o
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) -Wl,-rpath,$(LLVM_LIB_PATH) $^ $(LIBS)

issue.o: issue.cpp issue.hpp
	$(CXX) -c $(CXXFLAGS) $<

stylechecker.o: stylechecker.cpp issue.hpp 
	$(CXX) -c $(CXXFLAGS) $<

demo: demo.cpp
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS)
	#./demo demo-input.hpp --


CHECK_CXXFLAGS = $(filter-out -fno-exceptions, $(CXXFLAGS))

stylecheck: stylechecker
	echo "#!/bin/sh" > stylecheck
	echo $(shell pwd)'/stylechecker $$@ -- clang++ -c '$(CHECK_CXXFLAGS) $(CLANG_RUNTIME_INCLUDES) >> stylecheck
	chmod +x stylecheck

clean:
	rm -f stylechecker stylecheck *.o

test: stylechecker palindrome.cpp
	./stylechecker palindrome.cpp -- clang++ -c $(CXXFLAGS) $(CLANG_RUNTIME_INCLUDES)
