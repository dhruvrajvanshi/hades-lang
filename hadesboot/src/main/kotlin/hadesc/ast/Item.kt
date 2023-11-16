package hadesc.ast

/**
 * Things that can appear on the top level scope of a file/module
 */
sealed interface Item : Node {
    val name: Ident
    data class Fn(
        override val nodeData: NodeData,
        override val name: Ident,
        val body: Expr.Block,
        val signature: FnSignature,
    ): Item
}

data class FnSignature(
    val params: ArrayList<Param>
)

data class Param(
    override val nodeData: NodeData,
    val pat: Pat,
    val ty: Ty
): Node
