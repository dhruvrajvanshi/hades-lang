package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.types.Type

class IRBuilder {
    var position: IRBlock? = null

    fun buildRetVoid(): IRReturnVoidInstruction {
        return addStatement(IRReturnVoidInstruction)
    }

    fun buildConstBool(ty: Type, location: SourceLocation, value: Boolean): IRValue {
        return IRBool(ty, location, value)
    }


    fun <S : IRInstruction> addStatement(statement: S): S {
        val statements = requireNotNull(position).statements
        if (statements.isNotEmpty()) {
            require(statements.last() !is IRReturnInstruction) {
                "Tried to add statement after terminator"
            }
            require(statements.last() !is IRBr) {
                "Tried to add statement after terminator"
            }
        }
        statements.add(statement)
        return statement
    }

    fun buildByteString(ty: Type, location: SourceLocation, bytes: ByteArray): IRValue {
        return IRByteString(ty, location, bytes)
    }

    fun buildGetStructField(
            ty: Type,
            location: SourceLocation,
            lhs: IRValue,
            field: Name?,
            index: Int
    ): IRValue {
        return IRGetStructField(ty, location, lhs = lhs, rhs = field, index = index)
    }

    fun buildVariable(ty: Type, location: SourceLocation, name: IRName): IRValue {
        return IRVariable(ty, location, name)
    }

    fun buildMethodRef(
            type: Type,
            location: SourceLocation,
            thisArg: IRValue,
            method: IRValue
    ): IRValue {
        return IRMethodRef(
                type,
                location,
                thisArg,
                method
        )
    }

    fun buildCall(
            type: Type,
            location: SourceLocation,
            callee: IRValue,
            typeArgs: List<Type>?,
            args: List<IRValue>,
            name: IRLocalName
    ): IRValue {
        val call = IRCall(
                type = type,
                location = location,
                callee = callee,
                typeArgs = typeArgs,
                args = args,
                name = name
        )
        val ref = IRVariable(type, location, name)
        addStatement(call)
        return ref
    }

    fun buildReturn(value: IRValue): IRInstruction {
        return addStatement(IRReturnInstruction(value))
    }

    fun buildAlloca(type: Type, name: IRLocalName): IRInstruction {
        return addStatement(IRAlloca(type, name))
    }

    fun buildLoad(name: IRLocalName, type: Type, ptr: IRValue): IRInstruction {
        return addStatement(IRLoad(name, type, ptr))
    }

    fun buildStore(ptr: IRValue, value: IRValue): IRInstruction {
        return addStatement(IRStore(ptr, value))
    }

    fun buildBinOp(
            type: Type,
            name: IRLocalName,
            lhs: IRValue,
            operator: BinaryOperator,
            rhs: IRValue
    ): IRInstruction {
        return addStatement(IRBinOp(type, name, lhs, operator, rhs))
    }

    private fun positionAtEnd(block: IRBlock) {
        this.position = block
    }


    fun withinBlock(block: IRBlock, function: () -> Unit) {
        positionAtEnd(block)
        function()
    }

    fun buildNot(type: Type, location: SourceLocation, name: IRLocalName, value: IRValue): IRNot {
        return addStatement(IRNot(type, location, name, value))
    }

    fun buildBranch(location: SourceLocation, condition: IRValue, ifTrue: IRLocalName, ifFalse: IRLocalName): IRInstruction {
        return addStatement(IRBr(location, condition, ifTrue, ifFalse))
    }

    fun buildJump(location: SourceLocation, name: IRLocalName): IRInstruction {
        return addStatement(IRJump(location, name))
    }
}