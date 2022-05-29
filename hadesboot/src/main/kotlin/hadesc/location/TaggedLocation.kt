package hadesc.location

/**
 * Utility class for making type safe Maps of AST nodes
 * based on their location.
 *
 * A TaggedLocation<T> can be only created by passing in
 * a T, despite the fact that it doesn't actually care about
 * the T, only its location.
 * This way, we can have, for example, a Map<TaggedLocation<Binder>, Something>,
 * and be sure that we're only putting SourceLocations of Binder nodes
 * into this map.
 */
class TaggedLocation<T: HasLocation>(node: T) {
    val location = node.location

    override fun equals(other: Any?): Boolean = if (other is TaggedLocation<*>) {
        other.location == location
    } else false

    override fun hashCode(): Int = location.hashCode()
}

inline fun <reified T: HasLocation> T.taggedLocation() =
    TaggedLocation(this)
