package llvm

import org.bytedeco.llvm.global.LLVM

enum class IntPredicate(val value: Int) {
    SGE(LLVM.LLVMIntSGE),
    SGT(LLVM.LLVMIntSGT),
    SLT(LLVM.LLVMIntSLT),
    SLE(LLVM.LLVMIntSLE),

    UGE(LLVM.LLVMIntUGE),
    UGT(LLVM.LLVMIntUGT),
    ULT(LLVM.LLVMIntULT),
    ULE(LLVM.LLVMIntULE),

    EQ(LLVM.LLVMIntEQ),
    NE(LLVM.LLVMIntNE),
}