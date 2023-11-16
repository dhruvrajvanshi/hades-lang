package hadesc.ast

sealed interface Ty: Node {
    data class Tup(
        override val nodeData: NodeData,
        val types: List<Ty>,
    ): Ty
}