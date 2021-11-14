package llvm

import org.bytedeco.llvm.LLVM.LLVMBasicBlockRef
import org.bytedeco.llvm.LLVM.LLVMBuilderRef
import org.bytedeco.llvm.LLVM.LLVMTypeRef
import org.bytedeco.llvm.LLVM.LLVMValueRef
import org.bytedeco.llvm.global.LLVM

private typealias B = LLVMBuilderRef

val B.ref get() = this

fun B.positionAtEnd(block: LLVMBasicBlockRef) =
    LLVM.LLVMPositionBuilderAtEnd(this, block)

fun B.buildAlloca(type: LLVMTypeRef, name: String, alignment: Int): LLVMValueRef {
    val instr = LLVM.LLVMBuildAlloca(this, type, name)
    LLVM.LLVMSetAlignment(instr, alignment)
    return instr
}

fun B.buildLoad(ptr: LLVMValueRef, name: String): LLVMValueRef {
    return LLVM.LLVMBuildLoad(this, ptr, name)
}

fun B.buildStore(toPointer: LLVMValueRef, value: LLVMValueRef): LLVMValueRef {
    return LLVM.LLVMBuildStore(this, value, toPointer)
}

fun B.buildStructGEP(pointer: LLVMValueRef, index: Int, name: String): LLVMValueRef =
    LLVM.LLVMBuildStructGEP(this, pointer, index, name)

fun B.buildRet(value: LLVMValueRef): LLVMValueRef =
    LLVM.LLVMBuildRet(this, value)

fun B.buildRetVoid(): LLVMValueRef =
    LLVM.LLVMBuildRetVoid(this)

fun B.buildICmp(predicate: IntPredicate, lhs: LLVMValueRef, rhs: LLVMValueRef, name: String): LLVMValueRef =
    LLVM.LLVMBuildICmp(this, predicate.value, lhs, rhs, name)

fun B.buildBinOp(operator: Opcode, lhs: LLVMValueRef, rhs: LLVMValueRef, name: String): LLVMValueRef =
    LLVM.LLVMBuildBinOp(this, operator.value, lhs, rhs, name)

fun B.buildCondBr(condition: LLVMValueRef, ifTrue: LLVMBasicBlockRef, ifFalse: LLVMBasicBlockRef): LLVMValueRef =
    LLVM.LLVMBuildCondBr(this, condition, ifTrue, ifFalse)

fun B.buildSwitch(condition: LLVMValueRef, cases: List<Pair<LLVMValueRef, LLVMBasicBlockRef>>, elseBlock: LLVMBasicBlockRef): LLVMValueRef {
    val sw = LLVM.LLVMBuildSwitch(this, condition, elseBlock, cases.size)
    for (case in cases) {
        LLVM.LLVMAddCase(sw, case.first, case.second)
    }
    return sw
}

fun B.buildBr(destination: LLVMBasicBlockRef) =
    LLVM.LLVMBuildBr(this, destination)

fun B.buildNot(condition: LLVMValueRef, name: String): LLVMValueRef =
    LLVM.LLVMBuildNot(this, condition, name)

fun B.buildExtractValue(value: Value, index: Int, name: String): Value =
    LLVM.LLVMBuildExtractValue(
        this,
        value,
        index,
        name
    )

fun B.buildExtractElement(value: Value, index: Value, name: String): Value =
    LLVM.LLVMBuildExtractElement(this, value, index, name)

fun B.buildCall(callee: Value, args: List<Value>, name: String?): LLVMValueRef =
    LLVM.LLVMBuildCall(this, callee, args.asPointerPointer(), args.size, name ?: "")

fun B.buildZExt(value: Value, toType: Type, name: String): LLVMValueRef =
    LLVM.LLVMBuildZExt(this, value, toType, name)

fun B.buildTrunc(value: Value, toType: Type, name: String): LLVMValueRef =
    LLVM.LLVMBuildTrunc(this, value, toType, name)

fun B.buildBitCast(value: Value, toType: Type, name: String): LLVMValueRef =
    LLVM.LLVMBuildBitCast(this, value, toType, name)

fun B.buildPointerCast(value: Value, toType: Type, name: String): LLVMValueRef =
    LLVM.LLVMBuildPointerCast(this, value, toType, name)