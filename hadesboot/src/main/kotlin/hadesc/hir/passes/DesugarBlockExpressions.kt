package hadesc.hir.passes

import hadesc.context.NamingContext
import hadesc.hir.*

class DesugarBlockExpressions(override val namingCtx: NamingContext): AbstractHIRTransformer() {
}