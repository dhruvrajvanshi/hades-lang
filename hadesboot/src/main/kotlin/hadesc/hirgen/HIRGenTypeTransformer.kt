package hadesc.hirgen

import hadesc.context.Context
import hadesc.hir.TypeTransformer
import hadesc.types.Type

@Suppress("Unused")
class HIRGenTypeTransformer(private val ctx: Context) : TypeTransformer {
    override fun lowerType(type: Type): Type = when {
        else -> super.lowerType(type)
    }
}
