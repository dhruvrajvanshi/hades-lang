package hadesc.ide

import hadesc.Name
import hadesc.ide.queries.HIDEQuery

class HIDEContext {
    val ctx = this

    fun <T> rootQuery(q: HIDEQuery<T>): T {
        return q.run(ctx)
    }

    private val cache = mutableMapOf<HIDEQuery<*>, Any>()
    /**
     * Run [query] query for the purpose of calculating [dependentQuery] result.
     * Adds an edge between [dependentQuery] and [query] so that we can automatically
     * recompute [dependentQuery] when [query]'s dependencies have changed
     */
    fun <DependencyValue, DependentQuery : HIDEQuery<*>, DependencyQuery : HIDEQuery<DependencyValue>>
            query(dependentQuery: DependentQuery, query: DependencyQuery): DependencyValue {
        return cache.getOrPut(query) {
            query.run(this) as Any
        } as DependencyValue
    }

    fun makeName(text: String): Name = Name(text)

    private var _nameIndex = 0
    fun makeUniqueName(): Name {
        _nameIndex++
        return makeName("$_nameIndex")
    }
}
