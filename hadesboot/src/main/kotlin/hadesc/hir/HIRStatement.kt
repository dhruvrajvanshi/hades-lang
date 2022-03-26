package hadesc.hir

import hadesc.Name
import hadesc.location.SourceLocation
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
    data class Expression(
            val expression: HIRExpression
    ) : HIRStatement {
        override val location: SourceLocation get() = expression.location
    }

    data class Return(
            override val location: SourceLocation,
            val expression: HIRExpression
    ): HIRStatement

    data class Alloca(
            override val location: SourceLocation,
            override val name: Name,
            val isMutable: Boolean,
            val type: Type
    ) : HIRStatement, NameBinder {
        val pointerType get(): Type =
            type.ptr(isMutable)
    }

    data class Call(
        override val location: SourceLocation,
        val resultType: Type,
        override val name: Name,
        val callee: HIRExpression,
        val args: List<HIRExpression>
    ) : HIRStatement, NameBinder

    @Deprecated("Use Store")
    data class Assignment(
            override val location: SourceLocation,
            val name: Name,
            val value: HIRExpression
    ) : HIRStatement

    data class Store(
            override val location: SourceLocation,
            val ptr: HIRExpression,
            val value: HIRExpression
    ) : HIRStatement

    data class MatchInt(
        override val location: SourceLocation,
        val value: HIRExpression,
        val arms: List<MatchIntArm>,
        val otherwise: HIRBlock
    ) : HIRStatement

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
    ) : HIRStatement

    override fun prettyPrint(): String {
        return "${prettyPrintInternal()} // $location"
    }

    data class SwitchInt(
        override val location: SourceLocation,
        val condition: HIRExpression,
        val cases: List<SwitchIntCase>,
        val otherwise: Name
    ) : HIRStatement


    private fun prettyPrintInternal(): String = when(this) {
        is Call -> {
            "%${name.text}: ${resultType.prettyPrint()} = ${callee.prettyPrint()}(${args.joinToString(", ") { it.prettyPrint() } })"
        }
        is Expression -> expression.prettyPrint()
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
}

data class SwitchIntCase(
    val value: HIRConstant.IntValue,
    val block: Name
)

data class MatchIntArm(
    val value: HIRConstant.IntValue,
    val block: HIRBlock
)