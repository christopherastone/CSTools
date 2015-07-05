include ../llvm.mk
include ../clang.mk

CXX = clang++
BOOST_LIBS = -lboost_filesystem -lboost_regex -lboost_system

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 $(filter-out -fno-exceptions, $(LLVM_CXXFLAGS))
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS) $(BOOST_LIBS)


all: stylechecker stylecheck

stylechecker: stylechecker.o issue.o
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS)

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
	rm -f stylechecker check *.o

test: stylechecker palindrome.cpp
	./stylechecker palindrome.cpp -- clang++ -c $(CXXFLAGS) $(CLANG_RUNTIME_INCLUDES)
