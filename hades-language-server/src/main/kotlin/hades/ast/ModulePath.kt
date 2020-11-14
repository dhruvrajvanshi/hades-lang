package hades.ast

data class ModulePath(
    override val meta: ASTMeta,
    val head: Identifier,
    val tail: List<Identifier>,
) : ASTNode {
    fun prettyPrint(): String = if (tail.isEmpty()) head.name.text
    else head.name.text +
            "." + tail.joinToString(".") { it.name.text }

}