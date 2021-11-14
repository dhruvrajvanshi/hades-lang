package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

interface NodeMap<T: HasLocation, V> {
    operator fun get(key: T): V?
}
class MutableNodeMap<T : HasLocation, V>: NodeMap<T, V> {
    private val map = mutableMapOf<SourceLocation, Pair<T, V>>()

    fun getOrPut(key: T, compute: () -> V): V {
        val existing = map[key.location]
        if (existing != null) {
            return existing.second
        }
        val value = compute()
        map[key.location] = key to value
        return value
    }

    override operator fun get(key: T): V? {
        return map[key.location]?.second
    }

    operator fun set(key: T, value: V) {
        map[key.location] = key to value
    }

    val entries get() = map.entries.asSequence().map { it.value }
}
