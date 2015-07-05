CSTOOLS_ROOT := $(shell git rev-parse --show-toplevel)
LLVM_SRC_PATH   := $(CSTOOLS_ROOT)/llvm
LLVM_BUILD_PATH := $(CSTOOLS_ROOT)/llvm-build
LLVM_INSTALL_PATH := $(CSTOOLS_ROOT)/llvm-install



#$(info -----------------------------------------------)
#$(info Using LLVM_SRC_PATH = $(LLVM_SRC_PATH))
#$(info Using LLVM_BUILD_PATH = $(LLVM_BUILD_PATH))
#$(info Using LLVM_BIN_PATH = $(LLVM_BIN_PATH))
#$(info -----------------------------------------------)

LLVM_BIN_PATH   := $(LLVM_INSTALL_PATH)/bin
LLVM_CONFIG := $(LLVM_BIN_PATH)/llvm-config

LLVM_CPPFLAGS += $(shell $(LLVM_CONFIG) --cppflags)
LLVM_CXXFLAGS += $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LDFLAGS += $(shell $(LLVM_CONFIG) --ldflags) 

# Include these *after* clang's libraries
LLVM_LIBS += $(shell $(LLVM_CONFIG) --libs --system-libs asmparser bitreader support mc mcparser option)

