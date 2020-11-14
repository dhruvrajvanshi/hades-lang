package hades.ast

import javax.swing.plaf.nimbus.State


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
}