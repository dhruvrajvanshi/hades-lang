package hadesc.hirgen

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.TypeTransformer
import hadesc.types.Type

class HIRGenTypeTransformer(private val ctx: Context) : TypeTransformer {
    override fun lowerType(type: Type): Type = when {
        else -> super.lowerType(type)
    }

}
