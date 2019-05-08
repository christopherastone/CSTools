CLANG_VERSION = $(shell ls -t $(LLVM_LIB_PATH)/clang | head -1)
CLANG_INCLUDE_DIR = $(LLVM_INSTALL_PATH)/lib/clang/$(CLANG_VERSION)/include

#CPP_INCLUDE_DIR = $(shell echo | /usr/bin/clang++ -Wp,-v -x c++ -stdlib=libc++ -fsyntax-only - 2>&1 | egrep "^ " | head -1)
#CPP_INCLUDE_DIR = $(shell echo | clang++ -Wp,-v -x c++ -stdlib=libc++ -fsyntax-only - 2>&1 | egrep "^ " | grep "v1" | head -1)
#CPP_INCLUDE_DIR = /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/
CPP_INCLUDE_DIR=/usr/local/opt/llvm/include/c++/v1
