include ../llvm.mk
include ../clang.mk

CXX = clang++
BOOST_LIBS = -lboost_filesystem -lboost_system

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++14 -stdlib=libc++ $(LLVM_CXXFLAGS)
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS) $(BOOST_LIBS)


all: stylecheck

stylecheck: stylecheck.o issue.o
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS)

issue.o: issue.cpp issue.hpp
	$(CXX) -c $(CXXFLAGS) $<

stylecheck.o: stylecheck.cpp issue.hpp 
	$(CXX) -c $(CXXFLAGS) $<

demo: demo.cpp
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS)
	#./demo demo-input.hpp --


CHECK_CXXFLAGS = $(filter-out -fno-exceptions, $(CXXFLAGS))

check: stylecheck
	echo "#!/bin/sh" > check
	echo $(shell pwd)'/stylecheck $$@ -- clang++ -c '$(CHECK_CXXFLAGS) $(CLANG_RUNTIME_INCLUDES) >> check
	chmod +x check

clean:
	rm -f stylecheck check *.o

test: stylecheck palindrome.cpp
	./stylecheck palindrome.cpp -- clang++ -c $(CXXFLAGS) $(CLANG_RUNTIME_INCLUDES)
