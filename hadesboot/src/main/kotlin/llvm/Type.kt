package llvm

import org.bytedeco.llvm.LLVM.LLVMTypeRef
import org.bytedeco.llvm.global.LLVM

private typealias T = LLVMTypeRef

val T.ref get() = this

fun FunctionType(returns: Type, types: List<Type>, variadic: Boolean): Type {
    return LLVM.LLVMFunctionType(
        returns,
        types.asPointerPointer(),
        types.size,
        variadic.toLLVMBool()
    )
}

fun PointerType(type: T, addressSpace: Int = 0): Type = LLVM.LLVMPointerType(type, addressSpace)

fun T.getConstantNullPointer(): Value =
    LLVM.LLVMConstPointerNull(this)

fun IntType(size: Int, context: Context = LLVM.LLVMGetGlobalContext()) =
    LLVM.LLVMIntTypeInContext(context, size)

fun VoidType(context: Context = LLVM.LLVMGetGlobalContext()) =
    LLVM.LLVMVoidTypeInContext(context)

fun StructType(name: String, context: Context) =
    LLVM.LLVMStructCreateNamed(context, name)

fun T.setBody(elementTypes: List<Type>, packed: Boolean) =
    LLVM.LLVMStructSetBody(this, elementTypes.asPointerPointer(), elementTypes.size, packed.toLLVMBool())

