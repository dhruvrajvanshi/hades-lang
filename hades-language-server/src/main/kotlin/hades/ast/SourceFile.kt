package hades.ast

data class SourceFile(
    override val meta: ASTMeta,
    val definitions: List<Definition>,
) : ASTNode