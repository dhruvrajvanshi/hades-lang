package hadesc.ast

sealed interface Pat: Node {
    data class Ident(
        override val nodeData: NodeData,
        val ident: hadesc.ast.Ident
    ): Pat
}