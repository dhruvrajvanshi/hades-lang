package hadesc.ide

import hadesc.Name
import hadesc.ide.queries.HIDEQuery
import kotlin.reflect.KClass

class HIDEContext {
    val ctx = this

    fun <T> rootQuery(q: HIDEQuery<T>): T {
        return q.run(ctx)
    }

    /**
     * Run [query] query for the purpose of calculating [dependentQuery] result.
     * Adds an edge between [dependentQuery] and [query] so that we can automatically
     * recompute [dependentQuery] when [query]'s dependencies have changed
     */
    fun <DependencyValue, DependentQuery : HIDEQuery<*>, DependencyQuery : HIDEQuery<DependencyValue>>
            query(dependentQuery: DependentQuery, query: DependencyQuery): DependencyValue {
        return rootQuery(query)
    }

    fun makeName(text: String): Name = Name(text)

    private var _nameIndex = 0
    fun makeUniqueName(): Name {
        _nameIndex++
        return makeName("$_nameIndex")
    }

    private val classCaches = mutableMapOf<Pair<KClass<*>, String>, MutableMap<*, *>>()
    fun <K, V> getCache(clazz: KClass<*>, name: String): MutableMap<K, V> {
        return classCaches.getOrPut(clazz to name) { mutableMapOf<Any, Any>() } as MutableMap<K, V>
    }
}
