package llvm

import org.bytedeco.llvm.LLVM.LLVMContextRef
import org.bytedeco.llvm.global.LLVM

private typealias C = LLVMContextRef

fun C.dispose() = LLVM.LLVMContextDispose(this)
