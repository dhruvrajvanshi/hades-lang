package hadesc.ast

sealed interface Expr: Node {
    data class Block(
        override val nodeData: NodeData,
    ): Expr
}