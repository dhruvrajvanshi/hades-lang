package hadesc.ir

import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRInstruction {
    override fun toString(): String = prettyPrint()

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRReturnInstruction -> "return ${value.prettyPrint()}"
        is IRReturnVoidInstruction -> "return void"
        is IRCall -> {
            val typeArgs = if (typeArgs == null) {
                ""
            } else {
                "[${typeArgs.joinToString(", ") { it.prettyPrint() }}]"
            }
            val args = "(${this.args.joinToString(", ") { it.prettyPrint() }})"
            "${name.prettyPrint()}: ${type.prettyPrint()} = call ${type.prettyPrint()} ${callee.prettyPrint()}${typeArgs}${args}"
        }
        is IRAlloca -> "${name.prettyPrint()}: ${Type.Ptr(type, isMutable = true).prettyPrint()} = alloca ${type.prettyPrint()}"
        is IRStore -> "store ${ptr.prettyPrint()} ${value.prettyPrint()}"
        is IRLoad -> "${name.prettyPrint()}: ${type.prettyPrint()} = load ${ptr.prettyPrint()}"
        is IRNot -> "${name.prettyPrint()}: ${type.prettyPrint()} = not ${arg.prettyPrint()}"
        is IRBr -> "br ${condition.prettyPrint()} then:${ifTrue.prettyPrint()} else:${ifFalse.prettyPrint()}"
        is IRJump -> "jmp ${label.prettyPrint()}"
        is IRBinOp -> "${name.prettyPrint()}: ${type.prettyPrint()} = ${operator.prettyPrint()} ${lhs.prettyPrint()} ${rhs.prettyPrint()}"
        is IRSwitch -> "switch (${onValue.prettyPrint()}, ${cases.joinToString(", ") { it.prettyPrint() }})"
    }
}

class IRReturnInstruction(
        val value: IRValue
) : IRInstruction()

class IRAlloca(
        val type: Type,
        val name: IRLocalName
) : IRInstruction()

class IRStore(
        val ptr: IRValue,
        val value: IRValue
) : IRInstruction()

class IRLoad(
        val name: IRLocalName,
        val type: Type,
        val ptr: IRValue
) : IRInstruction()

object IRReturnVoidInstruction : IRInstruction()

class IRBinOp(
        val type: Type,
        val name: IRLocalName,
        val lhs: IRValue,
        val operator: BinaryOperator,
        val rhs: IRValue
) : IRInstruction()


data class IRCall(
        val type: Type,
        val location: SourceLocation,
        val callee: IRValue,
        val typeArgs: List<Type>?,
        val args: List<IRValue>,
        val name: IRLocalName
) : IRInstruction()

data class IRNot(
        val type: Type,
        val location: SourceLocation,
        val name: IRLocalName,
        val arg: IRValue
) : IRInstruction()

data class IRBr(
        val location: SourceLocation,
        val condition: IRValue,
        val ifTrue: IRLocalName,
        val ifFalse: IRLocalName
) : IRInstruction()

data class IRJump(
        val location: SourceLocation,
        val label: IRLocalName
) : IRInstruction()

data class IRSwitch(
    val location: SourceLocation,
    val onValue: IRValue,
    val cases: List<IRLocalName>
) : IRInstruction()

