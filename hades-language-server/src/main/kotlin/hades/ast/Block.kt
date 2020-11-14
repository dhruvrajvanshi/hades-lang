package hades.ast

data class Block(
    override val meta: ASTMeta,
    val statements: List<Statement>,
) : ASTNode