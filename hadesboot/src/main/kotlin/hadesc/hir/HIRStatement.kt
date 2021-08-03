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

    data class If(
            override val location: SourceLocation,
            val condition: HIRExpression,
            val trueBranch: HIRBlock,
            val falseBranch: HIRBlock
    ) : HIRStatement

    data class While(
            override val location: SourceLocation,
            val condition: HIRExpression,
            val body: HIRBlock
    ) : HIRStatement

    override fun prettyPrint(): String {
        return "${prettyPrintInternal()} // $location"
    }


    data class Branch(
        override val location: SourceLocation,
        val toBranchName: Name
    ) : HIRStatement

    data class ConditionalBranch(
        override val location: SourceLocation,
        val condition: HIRExpression,
        val trueBranchName: Name,
        val falseBranchName: Name
    ) : HIRStatement


    private fun prettyPrintInternal(): String = when(this) {
        is Expression -> expression.prettyPrint()
        is Return -> "return ${expression.prettyPrint()}"
        is ReturnVoid -> "return"
        is ValDeclaration -> "val ${name.text}: ${type.prettyPrint()}"
        is Assignment -> "${name.text} = ${value.prettyPrint()}"
        is If -> "if ${condition.prettyPrint()} ${trueBranch.prettyPrint()}\nelse ${falseBranch.prettyPrint()}"
        is While -> "while ${condition.prettyPrint()} ${body.prettyPrint()}"
        is Store -> "store ${ptr.prettyPrint()} = ${value.prettyPrint()}"
        is Branch -> "br ${toBranchName.text}"
        is ConditionalBranch -> "br if ${condition.prettyPrint()} then ${trueBranchName.text} else ${falseBranchName.text}"
    }
}