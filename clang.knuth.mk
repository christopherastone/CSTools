#CLANG_VERSION = $(shell ls -t $(LLVM_LIB_PATH)/clang | head -1)
#CLANG_INCLUDE_DIR = $(LLVM_INSTALL_PATH)/lib/clang/$(CLANG_VERSION)/include
CLANG_INCLUDE_DIR = $(LLVM_INSTALL_PATH)/include/clang/
CPP_INCLUDE_DIR = /usr/lib64/clang/6.0.1/include
