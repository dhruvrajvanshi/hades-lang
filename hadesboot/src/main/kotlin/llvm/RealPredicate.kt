package llvm

import org.bytedeco.llvm.global.LLVM

enum class RealPredicate(val value: Int) {
    UGE(LLVM.LLVMRealUGE),
    UGT(LLVM.LLVMRealUGT),
    ULT(LLVM.LLVMRealULT),
    ULE(LLVM.LLVMRealULE),

    EQ(LLVM.LLVMRealUEQ),
    NE(LLVM.LLVMRealUNE)
}
