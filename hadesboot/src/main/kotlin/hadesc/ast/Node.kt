package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

@JvmInline
value class NodeId(val value: Int)

data class NodeData(
    val id: NodeId,
    override val location: SourceLocation,
): HasLocation
sealed interface Node: HasLocation {
    val nodeData: NodeData

    override val location get() = nodeData.location
}
