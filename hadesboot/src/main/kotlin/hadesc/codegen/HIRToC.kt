package hadesc.codegen

import hadesc.context.Context
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRModule

class HIRToC(
    private val ctx: Context,
    private val module: HIRModule
)  {
    private val cDecls = mutableListOf<CDecl>()
    fun run() {
        for (def in module.definitions) {
            visitDef(def)
        }
    }

    private fun visitDef(def: HIRDefinition): Unit = when (def) {
        is HIRDefinition.Const -> TODO()
        is HIRDefinition.ExternConst -> TODO()
        is HIRDefinition.ExternFunction -> TODO()
        is HIRDefinition.Function -> TODO()
        is HIRDefinition.Implementation -> TODO()
        is HIRDefinition.Struct -> TODO()
    }
}

data class CDecl(val text: String)