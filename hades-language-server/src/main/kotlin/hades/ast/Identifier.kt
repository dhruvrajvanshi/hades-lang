package hades.ast

data class BindingIdentifier(
    override val meta: ASTMeta,
    val name: Name
) : ASTNode

data class Identifier(
    override val meta: ASTMeta,
    val name: Name,
) : ASTNode