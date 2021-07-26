package llvm

import org.bytedeco.llvm.global.LLVM

@Suppress("unused")
enum class Opcode(val value: Int) {
        Add(LLVM.LLVMAdd),
        Sub(LLVM.LLVMSub),
        Mul(LLVM.LLVMMul),
        SDiv(LLVM.LLVMSDiv),
        UDiv(LLVM.LLVMUDiv),
        And(LLVM.LLVMAnd),
        Or(LLVM.LLVMOr),
}