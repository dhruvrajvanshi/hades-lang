package hades.ast



sealed class Statement : ASTNode {
    data class Val(
        override val meta: ASTMeta,
        val name: BindingIdentifier,
        val typeAnnotation: TypeAnnotation?,
        val initializer: Expression,
    ) : Statement()

    data class Defer(
        override val meta: ASTMeta,
        val statement: Statement,
    ) : Statement()

    data class Call(
        val expression: Expression.Call,
    ) : Statement() {
        override val meta: ASTMeta
            get() = expression.meta
    }

    data class Error(override val meta: ASTMeta) : Statement()
}