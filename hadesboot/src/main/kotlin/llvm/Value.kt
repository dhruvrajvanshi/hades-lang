package llvm

import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM

private typealias V = LLVMValueRef

inline val V.ref get() = this

fun V.getType(): LLVMTypeRef =
    LLVM.LLVMTypeOf(this)

fun constantStruct(type: Type, values: List<Value>): Value {
    return LLVM.LLVMConstNamedStruct(type, values.asPointerPointer(), values.size)
}

fun constantInt(type: Type, value: Long, signExtend: Boolean = true): V =
    LLVM.LLVMConstInt(type, value, signExtend.toLLVMBool())

fun constantFloat(type: Type, value: Double): V =
    LLVM.LLVMConstReal(type, value)

fun V.asMetadata(): LLVMMetadataRef =
    LLVM.LLVMValueAsMetadata(this)

fun constantArray(itemType: Type, items: List<V>, length: Int): V =
    LLVM.LLVMConstArray(itemType, items.asPointerPointer(), length)

fun constantString(text: String, dontNullTerminate: Boolean = false, context: LLVMContextRef = LLVM.LLVMGetGlobalContext()): LLVMValueRef =
    LLVM.LLVMConstStringInContext(context, text, text.length, dontNullTerminate.toLLVMBool())

fun V.getInitializer(): LLVMValueRef? = LLVM.LLVMGetInitializer(this)
fun V.setInitializer(value: LLVMValueRef) = LLVM.LLVMSetInitializer(this, value)

fun V.getName(): String? = LLVM.LLVMGetValueName(this)?.string

fun V.createBlock(name: String): LLVMBasicBlockRef =
    LLVM.LLVMAppendBasicBlock(this, name)

fun V.prettyPrint(): String = LLVM.LLVMPrintValueToString(this).string

fun V.asFunctionValue() = this

fun V.getParameter(index: Int): LLVMValueRef =
    checkNotNull(LLVM.LLVMGetParam(this, index))
