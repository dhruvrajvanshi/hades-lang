package hades.ast

interface ASTNode: HasSourceRange {
    val meta: ASTMeta

    override val range get() = meta.range
}