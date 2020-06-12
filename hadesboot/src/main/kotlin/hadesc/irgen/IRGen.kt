package hadesc.irgen

import hadesc.context.Context
import hadesc.ir.IRModule

class IRGen(val ctx: Context) {
    fun generate(): IRModule {
        return ProgramVisitor(ctx).generate()
    }
}