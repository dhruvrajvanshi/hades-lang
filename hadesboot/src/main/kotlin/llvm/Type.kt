package llvm

import hadesc.assertions.requireUnreachable
import org.bytedeco.llvm.LLVM.LLVMTypeRef
import org.bytedeco.llvm.global.LLVM

private typealias T = LLVMTypeRef

val T.ref get() = this

fun functionType(returns: Type, types: List<Type>, variadic: Boolean): Type {
    return LLVM.LLVMFunctionType(
        returns,
        types.asPointerPointer(),
        types.size,
        variadic.toLLVMBool()
    )
}

fun pointerType(type: T, addressSpace: Int = 0): Type = LLVM.LLVMPointerType(type, addressSpace)

fun T.getConstantNullPointer(): Value =
    LLVM.LLVMConstPointerNull(this)

fun intType(size: Int, context: Context = LLVM.LLVMGetGlobalContext()): LLVMTypeRef =
    LLVM.LLVMIntTypeInContext(context, size)

fun floatType(size: Int, context: Context): T = when(size) {
    16 -> LLVM.LLVMHalfTypeInContext(context)
    32 -> LLVM.LLVMFloatTypeInContext(context)
    64 -> LLVM.LLVMDoubleTypeInContext(context)
    else -> requireUnreachable()
}

fun voidType(context: Context = LLVM.LLVMGetGlobalContext()): T =
    LLVM.LLVMVoidTypeInContext(context)

fun structType(name: String, context: Context): T =
    LLVM.LLVMStructCreateNamed(context, name)

fun T.setBody(elementTypes: List<Type>, packed: Boolean) =
    LLVM.LLVMStructSetBody(this, elementTypes.asPointerPointer(), elementTypes.size, packed.toLLVMBool())

