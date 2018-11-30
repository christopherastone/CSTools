include ../llvm.mk
include ../clang.mk

CXX = $(LLVM_BIN_PATH)/clang++    
##CXX = clang++    

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 $(LLVM_CXXFLAGS) 
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS)

CLANGCOMMAND = clang++ $(CLANG_RUNTIME_INCLUDES) -std=c++11 -c

TARGETS = cpptool dump run 
ALLTARGETS = compile_commands.json

# the LLVM_CONFIG dependency is just there to double-check that
# we have our path set correctly
all: $(LLVM_CONFIG) $(ALLTARGETS)

cpptool: cpptool.o 
	@echo Linking $@
	@$(CXX) -o $@ $(LDFLAGS) -Wl,-rpath,$(LLVM_LIB_PATH) $^ $(LIBS)

cpptool.o: cpptool.cpp 
	@echo Compiling $*.cpp
	@$(CXX) -c $(CXXFLAGS) $<

dump: ../llvm.mk ../clang.mk Makefile
	@echo Generating dump script
	@rm -f dump
	@touch dump
	@echo '#!/bin/zsh' > dump
	@echo ${shell pwd}/cpptool -d $$\* -- ${CLANGCOMMAND} -fparse-all-comments -I $(LLVM_INSTALL_PATH)/include | cat >> dump
	@chmod +x dump

# if we use the default shell, quoted arguments containing spaces get screwed up when the cpptool command runs
# So, make sure the script runs zsh
run: ../llvm.mk ../clang.mk Makefile
	@echo Generating run script
	@rm -f run
	@touch run
	@echo '#!/bin/zsh' > run
	cat run
	@echo ${shell pwd}/cpptool $$\* -- ${CLANGCOMMAND} -fparse-all-comments -I $(LLVM_INSTALL_PATH)/include | cat >> run
	@chmod +x run

compile_commands.json: Makefile
	make clean
	compiledb make $(TARGETS)


clean:
	rm -f cpptool cpptool.o

test: all
	(cd testing; $(MAKE) test)

