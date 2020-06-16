package llvm

import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM

typealias Type = LLVMTypeRef
typealias Value = LLVMValueRef
typealias Context = LLVMContextRef
typealias Module = LLVMModuleRef
typealias Builder = LLVMBuilderRef
typealias BasicBlock = LLVMBasicBlockRef
typealias FunctionValue = LLVMValueRef

