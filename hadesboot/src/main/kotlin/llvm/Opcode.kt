package llvm

import org.bytedeco.llvm.global.LLVM

@Suppress("unused")
enum class Opcode(val value: Int) {
        Add(LLVM.LLVMAdd),
        Sub(LLVM.LLVMSub),
        Mul(LLVM.LLVMMul),
        SDiv(LLVM.LLVMSDiv),
        UDiv(LLVM.LLVMUDiv),
        SRem(LLVM.LLVMSRem),
        URem(LLVM.LLVMURem),
        And(LLVM.LLVMAnd),
        Or(LLVM.LLVMOr),

        FAdd(LLVM.LLVMFAdd),
        FSub(LLVM.LLVMFSub),
        FMul(LLVM.LLVMFMul),
        FDiv(LLVM.LLVMFDiv),
        FRem(LLVM.LLVMFRem),
}