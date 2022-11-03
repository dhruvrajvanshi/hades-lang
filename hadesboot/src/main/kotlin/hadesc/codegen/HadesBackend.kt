package hadesc.codegen

import hadesc.context.Context
import hadesc.hir.HIRModule

sealed interface HadesBackend {
    fun generate(
        ctx: Context,
        module: HIRModule,
    )
    object LLVM: HadesBackend {
        override fun generate(ctx: Context, module: HIRModule) {
            LLVMToObject(ctx.options, ctx.target, HIRToLLVM(ctx, module).lower()).execute()
        }
    }
}
