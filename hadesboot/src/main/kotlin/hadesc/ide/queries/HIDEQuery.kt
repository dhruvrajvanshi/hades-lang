package hadesc.ide.queries

import hadesc.ide.HIDEContext

interface HIDEQuery<T> {
    fun run(ctx: HIDEContext): T
}