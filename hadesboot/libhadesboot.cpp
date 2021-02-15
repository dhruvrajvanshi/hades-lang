#include "llvm-c/Core.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <system_error>


using namespace llvm;

static Module::ModFlagBehavior
map_to_llvmModFlagBehavior(LLVMModuleFlagBehavior Behavior) {
  switch (Behavior) {
  case LLVMModuleFlagBehaviorError:
    return Module::ModFlagBehavior::Error;
  case LLVMModuleFlagBehaviorWarning:
    return Module::ModFlagBehavior::Warning;
  case LLVMModuleFlagBehaviorRequire:
    return Module::ModFlagBehavior::Require;
  case LLVMModuleFlagBehaviorOverride:
    return Module::ModFlagBehavior::Override;
  case LLVMModuleFlagBehaviorAppend:
    return Module::ModFlagBehavior::Append;
  case LLVMModuleFlagBehaviorAppendUnique:
    return Module::ModFlagBehavior::AppendUnique;
  }
}


extern "C" void LLVMAddModuleFlagi32(LLVMModuleRef M, LLVMModuleFlagBehavior Behavior,
                       const char *Key, size_t KeyLen,
                       uint32_t Val) {
  unwrap(M)->addModuleFlag(map_to_llvmModFlagBehavior(Behavior),
                           {Key, KeyLen}, Val);
}