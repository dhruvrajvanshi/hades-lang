package hadesc.hir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.types.ptr

sealed interface HIRStatement: HIRNode {
    /**
     * Instructions that bind a new name
     * e.g.
     * %x = alloca u32
     */
    sealed interface NameBinder {
        val name: Name
    }
    sealed interface Terminator

    /**
     * Instructions that can contain nested blocks.
     * Should not be present after lowering to a Basic block
     * (SimplifyControlFlow).
     * After lowering, these statements should be converted to
     * instructions that jump to block labels, instead of
     * owning a block themselves.
     * e.g. jump .some_label,
     * instead of
     *  jump {
     *    ...nested block
     *  }
     */
    sealed interface NestedControlFlow

    /**
     * Instructions that can do basic block control flow,
     * i.e. jump to block labels (can never hold blocks themselves).
     * This and NestedControlFlow combined are the only instructions
     * that can jump within a function.
     */
    sealed interface BasicBlockControlFlow: Terminator

    /**
     * Simple instructions that can't branch
     */
    sealed interface StraightLineInstruction

    data class Not(
        override val name: Name,
        val expression: HIRExpression
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        override val location get() = expression.location
    }

    data class Return(
            override val location: SourceLocation,
            val expression: HIRExpression
    ): HIRStatement, Terminator

    data class Alloca(
            override val location: SourceLocation,
            override val name: Name,
            val isMutable: Boolean,
            val type: Type
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        val pointerType get(): Type =
            type.ptr(isMutable)
    }

    data class Call(
        override val location: SourceLocation,
        val resultType: Type,
        override val name: Name,
        val callee: HIROperand,
        val args: List<HIRExpression>
    ) : HIRStatement, NameBinder, StraightLineInstruction

    data class Load(
        override val location: SourceLocation,
        override val name: Name,
        val ptr: HIROperand
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        init {
            require(ptr.type is Type.Ptr)
        }

        val ptrType get(): Type.Ptr {
            val t = ptr.type
            check(t is Type.Ptr)
            return t
        }
        val type get() = ptrType.to
    }

    data class PointerCast(
        override val location: SourceLocation,
        override val name: Name,
        val toPointerOfType: Type,
        val value: HIRExpression
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        val type: Type
            get() = Type.Ptr(toPointerOfType, isMutable = true)
    }


    @Deprecated("Use Store")
    data class Assignment(
            override val location: SourceLocation,
            val name: Name,
            val value: HIRExpression
    ) : HIRStatement, StraightLineInstruction

    data class Store(
            override val location: SourceLocation,
            val ptr: HIROperand,
            val value: HIRExpression
    ) : HIRStatement, StraightLineInstruction

    @Deprecated("Use basic block structured instructions")
    data class MatchInt(
        override val location: SourceLocation,
        val value: HIRExpression,
        val arms: List<MatchIntArm>,
        val otherwise: HIRBlock
    ) : HIRStatement, NestedControlFlow


    data class GetStructField(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type,
        val lhs: HIRExpression,
        val fieldName: Name,
        val index: Int
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        init {
            require(lhs.type !is Type.Ptr) {
                TODO()
            }
        }
    }

    data class GetStructFieldPointer(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type.Ptr,
        val lhs: HIRExpression,
        val memberName: Name,
        val memberIndex: Int
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        init {
            require(lhs.type is Type.Ptr)
        }
    }

    data class IntegerConvert(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type,
        val value: HIROperand
    ) : HIRStatement, NameBinder, StraightLineInstruction

    data class AllocateClosure(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type.Function,
        val function: HIROperand,
        val captures: List<HIROperand>,
    ): HIRStatement, NameBinder, StraightLineInstruction

    data class InvokeClosure(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type,
        val closureRef: HIROperand,
        val args: List<HIROperand>,
    ): HIRStatement, NameBinder, StraightLineInstruction

    data class GetCapturePointer(
        override val location: SourceLocation,
        val type: Type.Ptr,
        val captureName: Name,
        override val name: Name
    ): HIRStatement, NameBinder, StraightLineInstruction

    data class GetCaptureValue(
        override val location: SourceLocation,
        val type: Type,
        val captureName: Name,
        override val name: Name,
    ): HIRStatement, NameBinder, StraightLineInstruction

    /**
     * The basic structure of a while statement is this
     *
     * val [conditionName]: Bool // This statement isn't a part of this syntax node. It has to be separately generated
     * // [conditionBlock]
     * {
     *  ...
     *  // This must be the last statement of [conditionBlock]
     *  [conditionName] = ...
     * }
     * {
     *  [body]
     * }
     *
     * The slightly awkward structure of this node is to handle short-circuiting.
     * The obvious way to represent this would have been
     * While(val condition: [HIRExpression], val body: [HIRBlock])
     * The problem is that simplification of short-circuiting expressions
     * yields an expression that is a variable reference.
     *
     * while a and b { ... } -> c = a; if not c { c = b }; while c { ... }
     * This means that after this lowering, the evaluation of c moves out of
     * the While node if we're not careful.
     * Instead, if we just keep the evaluation logic of the condition as a block,
     * [hadesc.hir.passes.SimplifyShortCircuitingOperators] pass emit statements
     * inside the condition block, without changing the meaning of the code.
     * Right now, this is the simplest change I can think of to fix this bug.
     * See test/short_circuit_while for the pathological case. This used to
     * become an infinite loop after [hadesc.hir.passes.SimplifyShortCircuitingOperators]
     * was run. Ideally, [hadesc.hir.passes.SimplifyShortCircuitingOperators] should be
     * more careful, but this was simpler to implement and reason about.
     */
    @Deprecated("Use basic block structured instructions")
    data class While(
        override val location: SourceLocation,
        /**
         * Name of the condition variable to check while looping
         */
        val conditionName: Name,
        /**
         * Block of statements that evaluates the loop condition and
         * assigns true/false to [conditionName]
         */
        val conditionBlock: HIRBlock,
        val body: HIRBlock
    ) : HIRStatement, NestedControlFlow


    data class BinOp(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type,
        val lhs: HIRExpression,
        val operator: BinaryOperator,
        val rhs: HIRExpression
    ) : HIRStatement, NameBinder, StraightLineInstruction

    data class SwitchInt(
        override val location: SourceLocation,
        val condition: HIRExpression,
        val cases: List<SwitchIntCase>,
        val otherwise: Name
    ) : HIRStatement, Terminator, BasicBlockControlFlow

    data class Jump(
        override val location: SourceLocation,
        val to: Name
    ): HIRStatement, Terminator, BasicBlockControlFlow

    data class TypeApplication(
        override val location: SourceLocation,
        override val name: Name,
        val type: Type,
        val expression: HIROperand,
        val args: List<Type>
    ) : HIRStatement, NameBinder, StraightLineInstruction {
        init {
            require(expression.type is Type.TypeFunction || expression.type is Type.Constructor)
        }
    }

    private fun prettyPrintInternal(): String = when(this) {
        is Call -> {
            "%${name.text}: ${resultType.prettyPrint()} = ${callee.prettyPrint()}(${args.joinToString(", ") { it.prettyPrint() } })"
        }
        is Return -> "return ${expression.prettyPrint()}"
        is Alloca -> "%${name.text}: ${pointerType.prettyPrint()} = alloca ${type.prettyPrint()}"
        is Assignment -> "%${name.text} = ${value.prettyPrint()}"
        is MatchInt -> "match ${value.prettyPrint()} {\n    " +
                arms.joinToString("\n    ") { it.value.prettyPrint() + " -> ${it.block.prettyPrint().prependIndent("    ").trimStart()}" } +
                "\n    otherwise -> ${otherwise.prettyPrint().prependIndent("    ").trimStart()}\n" +
                "  }"
        is While -> "while ${conditionBlock.prettyPrint()} { $conditionName } ${body.prettyPrint()}"
        is Store -> "store ${ptr.prettyPrint()} = ${value.prettyPrint()}"
        is SwitchInt -> "switch ${condition.prettyPrint()} [\n    " +
                cases.joinToString { "case ${it.value.prettyPrint()}: ${it.block.text}\n    " } +
                "otherwise: ${otherwise.text}\n" +
                "  ]"
        is Load -> "%${name.text}: ${type.prettyPrint()} = load ${ptr.prettyPrint()}"
        is GetStructField -> "%${name.text}: ${type.prettyPrint()} = ${lhs.prettyPrint()}.${fieldName.text}"
        is GetStructFieldPointer -> "%${name.text}: ${type.prettyPrint()} = field-offset ${lhs.prettyPrint()} ${memberName.text}"
        is Not -> "%${name.text}: Bool = ${expression.prettyPrint()}"
        is Jump -> "jump ${to.text}"
        is IntegerConvert -> "%${name.text}: ${type.prettyPrint()} = integer_convert[${type.prettyPrint()}] ${value.prettyPrint()}"
        is TypeApplication -> {
            val typeArgsStr = "[${args.joinToString(", ") { it.prettyPrint() } }]"
            "%${name.text}: ${type.prettyPrint()} = ${expression.prettyPrint()}$typeArgsStr"
        }
        is PointerCast ->
            "%${name.text}: ${type.prettyPrint()} = pointer-cast[${toPointerOfType.prettyPrint()}] ${value.prettyPrint()}"
        is BinOp ->
            "%${name.text}: ${type.prettyPrint()} = ${operator.prettyPrint()} ${lhs.prettyPrint()}, ${rhs.prettyPrint()}"
        is AllocateClosure ->
            "%${name.text} = allocate closure " +
                    captures.joinToString(", ") { it.prettyPrint() } +
                    ", ${function.prettyPrint()}"
        is InvokeClosure -> "%${name.text}: ${type.prettyPrint()} = invoke closure " +
                "${closureRef.prettyPrint()}(${args.joinToString(", ") { it.prettyPrint() }})"
        is GetCapturePointer -> "%${name.text}: ${type.prettyPrint()} = get capture ptr ${captureName.text}"
        is GetCaptureValue -> "%${name.text}: ${type.prettyPrint()} = get capture value ${captureName.text}"
    }

    companion object {
        @JvmStatic
        fun ifStatement(
            location: SourceLocation,
            condition: HIRExpression,
            trueBranch: HIRBlock,
            falseBranch: HIRBlock
        ): HIRStatement {
            return MatchInt(
                location,
                condition,
                listOf(
                    MatchIntArm(
                        value = HIRConstant.IntValue(
                            location,
                            Type.Bool,
                            1
                        ),
                        trueBranch
                    )
                ),
                falseBranch
            )
        }
    }

    override fun prettyPrint(): String {
        return "${prettyPrintInternal()} // $location"
    }

}

data class SwitchIntCase(
    val value: HIRConstant.IntValue,
    val block: Name
)

data class MatchIntArm(
    val value: HIRConstant.IntValue,
    val block: HIRBlock
)