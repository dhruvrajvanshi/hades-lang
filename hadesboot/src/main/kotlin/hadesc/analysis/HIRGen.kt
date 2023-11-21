package hadesc.analysis

import hadesc.ast.SyntaxVisitor
import hadesc.context.SourceFileResolverCtx
import hadesc.hir.HIRModule

class HIRGen<Ctx>(
    private val ctx: Ctx
) : SyntaxVisitor where Ctx : SourceFileResolverCtx, Ctx: TyCtx {
    private val module = HIRModule(mutableListOf())
    fun lower(): HIRModule {
        ctx.forEachSourceFile {
            for (declaration in it.declarations) {
                visitDeclaration(declaration)
            }
        }
        return module
    }
}