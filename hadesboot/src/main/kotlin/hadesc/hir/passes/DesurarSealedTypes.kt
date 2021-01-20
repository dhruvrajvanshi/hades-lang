package hadesc.hir.passes

import hadesc.context.Context
import hadesc.hir.HIRModule
import hadesc.logging.logger

class DesurarSealedTypes(val ctx: Context) : HIRTransformer {

    override fun transformModule(oldModule: HIRModule): HIRModule {
        logger().info(oldModule.prettyPrint())
        return super.transformModule(oldModule)
    }
}