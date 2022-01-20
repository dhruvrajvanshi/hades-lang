package hadesc.hir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed interface HIRStatement: HIRNode {
    data class Expression(
            val expression: HIRExpression
    ) : HIRStatement {
        override val location: SourceLocation get() = expression.location
    }

    data class Return(
            override val location: SourceLocation,
            val expression: HIRExpression
    ): HIRStatement

    data class ReturnVoid(override val location: SourceLocation) : HIRStatement
    data class ValDeclaration(
            override val location: SourceLocation,
            val name: Name,
            val isMutable: Boolean,
            val type: Type
    ) : HIRStatement

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

    data class While(
            override val location: SourceLocation,
            val condition: HIRExpression,
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
        is Expression -> expression.prettyPrint()
        is Return -> "return ${expression.prettyPrint()}"
        is ReturnVoid -> "return"
        is ValDeclaration -> "val ${name.text}: ${type.prettyPrint()}"
        is Assignment -> "${name.text} = ${value.prettyPrint()}"
        is MatchInt -> "match ${value.prettyPrint()} {\n    " +
                arms.joinToString("\n    ") { it.value.prettyPrint() + " -> ${it.block.prettyPrint().prependIndent("    ").trimStart()}" } +
                "\n    otherwise -> ${otherwise.prettyPrint().prependIndent("    ").trimStart()}\n" +
                "  }"
        is While -> "while ${condition.prettyPrint()} ${body.prettyPrint()}"
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