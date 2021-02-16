package llvm

import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM


private typealias V = LLVMValueRef

inline val V.ref get() = this

fun V.getType(): LLVMTypeRef =
    LLVM.LLVMTypeOf(this)

fun ConstantStruct(type: Type, values: List<Value>): Value {
    return LLVM.LLVMConstNamedStruct(type, values.asPointerPointer(), values.size)
}

fun ConstantInt(type: Type, value: Long, signExtend: Boolean = true): V =
    LLVM.LLVMConstInt(type, value, signExtend.toLLVMBool())

fun V.asMetadata(): LLVMMetadataRef =
    LLVM.LLVMValueAsMetadata(this)

fun ConstantArray(text: String, nullTerminate: Boolean = false, context: LLVMContextRef = LLVM.LLVMGetGlobalContext()) =
    LLVM.LLVMConstStringInContext(context, text, text.length, nullTerminate.toLLVMBool())


fun V.getInitializer(): LLVMValueRef? = LLVM.LLVMGetInitializer(this)
fun V.setInitializer(value: LLVMValueRef) = LLVM.LLVMSetInitializer(this, value)

fun V.getName(): String? = LLVM.LLVMGetValueName(this)?.string


fun V.createBlock(name: String): LLVMBasicBlockRef =
    LLVM.LLVMAppendBasicBlock(this, name)

fun V.dumpToString(): String = LLVM.LLVMPrintValueToString(this).string

fun V.asFunctionValue() = this

fun V.getParameter(index: Int) =
    LLVM.LLVMGetParam(this, index)

