package llvm

import org.bytedeco.llvm.LLVM.LLVMMetadataRef
import org.bytedeco.llvm.global.LLVM

typealias Metadata = LLVMMetadataRef

fun Metadata.asValue(ctx: Context): Value =
    LLVM.LLVMMetadataAsValue(ctx, this)
