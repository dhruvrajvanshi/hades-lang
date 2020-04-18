package hadesc.ir.passes

import hadesc.context.Context
import hadesc.ir.IRModule
import hadesc.logging.logger

class LowerGenerics(val ctx: Context, val module: IRModule) {
    private val log = logger()
    fun run() {
        log.debug(module.prettyPrint())
    }
}