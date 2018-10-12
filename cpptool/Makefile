include ../llvm.mk
include ../clang.mk

CXX = $(LLVM_BIN_PATH)/clang++    

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 -stdlib=libc++ $(LLVM_CXXFLAGS)
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS)

CLANGCOMMAND = clang++ $(CLANG_RUNTIME_INCLUDES) -std=c++11 -stdlib=libc++ -c

TARGETS = cpptool dump

# the LLVM_CONFIG dependency is just there to double-check that
# we have our path set correctly
all: $(LLVM_CONFIG) $(TARGETS)

cpptool: cpptool.o 
	@echo Linking $@
	@$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

cpptool.o: cpptool.cpp 
	@echo Compiling $*.cpp
	@$(CXX) -c $(CXXFLAGS) $<

dump: ../llvm.mk ../clang.mk Makefile
	echo ${shell pwd}/cpptool -d $$\* -- ${CLANGCOMMAND} -I $(LLVM_INSTALL_PATH)/include | cat > dump
	chmod +x dump

clean:
	rm -f cpptool cpptool.o

test: all
	(cd testing; $(MAKE) test)
