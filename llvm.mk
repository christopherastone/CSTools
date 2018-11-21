CSTOOLS_ROOT := $(shell git rev-parse --show-toplevel)
include $(CSTOOLS_ROOT)/llvm.$(shell hostname -s).mk


#$(info -----------------------------------------------)
#$(info Using LLVM_SRC_PATH = $(LLVM_SRC_PATH))
#$(info Using LLVM_BUILD_PATH = $(LLVM_BUILD_PATH))
#$(info Using LLVM_BIN_PATH = $(LLVM_BIN_PATH))
#$(info -----------------------------------------------)

LLVM_BIN_PATH   := $(LLVM_INSTALL_PATH)/bin
LLVM_CONFIG := $(LLVM_BIN_PATH)/llvm-config

LLVM_CPPFLAGS += $(shell $(LLVM_CONFIG) --cppflags)
# For some reason llvm-config returns the -Wno-class-memaccess flag, but
#   clang++ itself doesn't understand it, leading to a pointless warning.
LLVM_CXXFLAGS += $(filter-out -Wno-class-memaccess, $(shell $(LLVM_CONFIG) --cxxflags))
#LLVM_CXXFLAGS += $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LDFLAGS += $(shell $(LLVM_CONFIG) --ldflags) 

# Include these *after* clang's libraries
LLVM_LIBS += $(shell $(LLVM_CONFIG) --libs --system-libs asmparser bitreader support mc mcparser option profiledata)

