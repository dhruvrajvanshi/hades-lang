package hades.ast

data class ASTMeta(
    override val range: SourceRange
) : HasSourceRange
