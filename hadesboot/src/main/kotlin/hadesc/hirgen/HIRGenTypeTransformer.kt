package hadesc.hirgen

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.TypeTransformer
import hadesc.types.Type

class HIRGenTypeTransformer(private val ctx: Context) : TypeTransformer {
    override fun lowerType(type: Type): Type = when {
        ctx.analyzer.isRefStructType(type) -> lowerRefStructType(type)
        else -> super.lowerType(type)
    }

    private fun lowerRefStructType(type: Type) = when (type) {
        is Type.Constructor -> Type.Ref(type)
        is Type.Application -> {
            check(type.callee is Type.Constructor)
            Type.Ref(
                Type.Application(
                    type.callee,
                    type.args.map { lowerType(it) }
                )
            )
        }

        else -> requireUnreachable()
    }

}
