include ../llvm.mk
include ../clang.mk

#CXX = $(LLVM_BIN_PATH)/clang++    
CXX = clang++    

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 $(LLVM_CXXFLAGS) 
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS)

CLANGCOMMAND = clang++ $(CLANG_RUNTIME_INCLUDES) -std=c++11 -c

TARGETS = cpptool dumpast run 
ALLTARGETS = compile_commands.json $(TARGETS) 

# the LLVM_CONFIG dependency is just there to double-check that
# we have our path set correctly
all: $(LLVM_CONFIG) $(ALLTARGETS)

cpptool: cpptool.o 
	@echo Linking $@
	@$(CXX) -o $@ $(LDFLAGS) -Wl,-rpath,$(LLVM_LIB_PATH) $^ $(LIBS)

cpptool.o: cpptool.cpp 
	@echo Compiling $*.cpp
	$(CXX) -c $(CXXFLAGS) $<

dumpast: ../llvm.mk ../clang.mk Makefile
	@echo Generating dumpast script
	@rm -f dumpast
	@touch dumpast
	@echo '#!/bin/zsh' > dumpast
	@echo ${CLANGCOMMAND} -Xclang -ast-dump $$\* -fsyntax-only -fparse-all-comments -I $(LLVM_INSTALL_PATH)/include | cat >> dumpast
	@chmod +x dumpast

# if we use the default shell, quoted arguments containing spaces get screwed up when the cpptool command runs
# So, make sure the script runs zsh
run: ../llvm.mk ../clang.mk Makefile
	@echo Generating run script
	@rm -f run
	@touch run
	@echo '#!/bin/zsh' > run
	@echo ${shell pwd}/cpptool $$\* -- ${CLANGCOMMAND} -fparse-all-comments -I $(LLVM_INSTALL_PATH)/include | cat >> run
	@chmod +x run

compile_commands.json: Makefile
	make clean
	compiledb make $(TARGETS)


clean:
	rm -f *.o $(TARGETS)

test: all
	(cd testing; $(MAKE) test)


test-calls: run cpptool
	./run -c -calls testing/cs70-intlist-good.cpp

test-extract: run cpptool
	./run -extract push_back -e testing/intlist.expect testing/cs70-intlist-good.cpp
	./run -extract push_back testing/cs70-intlist-good.cpp

test-dump: run cpptool
	./run -d testing/cs70-intlist-good.cpp

test-expect: run cpptool
	./run -e testing/intlist.expect testing/cs70-intlist-good.cpp
	./run -e testing/intlist.expect testing/cs70-intlist-bad4.cpp








