include ../llvm.mk
include ../clang.mk

CXX = $(LLVM_BIN_PATH)/clang++    

CPPFLAGS += $(LLVM_CPPFLAGS)
CXXFLAGS += -std=c++11 -stdlib=libc++ $(LLVM_CXXFLAGS)
LDFLAGS  += $(LLVM_LDFLAGS)
LIBS += $(CLANG_LIBS) $(LLVM_LIBS)

CLANGCOMMAND = clang++ $(CLANG_RUNTIME_INCLUDES) -std=c++11 -stdlib=libc++ -c


# the LLVM_CONFIG dependency is just there to double-check that
# we have our path set correctly
all: $(LLVM_CONFIG) replacer run

replacer: replacer.o 
	@echo Linking $@
	@$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

replacer.o: replacer.cpp 
	@echo Compiling $*.cpp
	@$(CXX) -c $(CXXFLAGS) $<

run: ../llvm.mk ../clang.mk
	echo ${shell pwd}/replacer $$\* -- ${CLANGCOMMAND} -I $(LLVM_INSTALL_PATH)/include | cat > run
	chmod +x run

clean:
	rm -f replacer replacer.o

test: all
	(cd testing; $(MAKE) test)

