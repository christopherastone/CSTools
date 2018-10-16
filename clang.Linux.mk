CLANG_VERSION = $(shell ls -t $(LLVM_LIB_PATH)/clang | head -1)
CLANG_INCLUDE_DIR = $(LLVM_INSTALL_PATH)/lib/clang/$(CLANG_VERSION)/include
