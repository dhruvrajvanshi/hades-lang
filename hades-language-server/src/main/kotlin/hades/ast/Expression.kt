package hades.ast

sealed class Expression : ASTNode {
    data class Variable(
        val name: Identifier,
    ) : Expression() {
        override val meta: ASTMeta
            get() = name.meta
    }

    data class DotProperty(
        override val meta: ASTMeta,
        val lhs: Expression,
        val rhs: Identifier,
    ) : Expression()

    data class Call(
        override val meta: ASTMeta,
        val callee: Expression,
        val args: List<Argument>,
    ) : Expression()

    data class TypeApplication(
        override val meta: ASTMeta,
        val callee: Expression,
        val args: List<TypeArgument>,
    ) : Expression()
}

data class Argument(
    override val meta: ASTMeta,
    val label: Identifier?,
    val value: Expression,
): ASTNode