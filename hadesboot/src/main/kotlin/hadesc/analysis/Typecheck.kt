package hadesc.analysis

import hadesc.ast.Declaration
import hadesc.ast.SyntaxVisitor
import hadesc.ast.TypeAnnotation
import hadesc.context.DiagnosticReporterCtx
import hadesc.context.ResolverCtx
import hadesc.context.SourceFileResolverCtx
import hadesc.diagnostics.Diagnostic
import hadesc.diagnostics.DiagnosticReporter
import hadesc.unit

class Typecheck<Ctx>(
    private val ctx: Ctx,
) where Ctx: SourceFileResolverCtx,
        Ctx: DiagnosticReporterCtx,
        Ctx: ResolverCtx
{
    fun typecheck() {
        ctx.forEachSourceFile { sourceFile ->
            for (declaration in sourceFile.declarations) {
                val infer = Infer.make()
                val checker = TypecheckImpl(
                    ctx,
                    infer,
                    ctx.diagnosticReporter
                )
                val errors = checker(declaration)
                errors.forEach {
                    ctx.diagnosticReporter.report(it.sourceLocation, it.kind)
                }
            }
        }
    }
}

private class TypecheckImpl<Ctx>(
    private val ctx: Ctx,
    private val infer: Infer,
    private val diagnostic: DiagnosticReporter,
) : SyntaxVisitor where Ctx: ResolverCtx {
    private val errors = mutableListOf<Diagnostic>()
    private val astConv = ASTConv(
        resolver = ctx.resolver,
        report = { kind, location ->
            errors.add(Diagnostic(location, kind))
        }
    )

    operator fun invoke(def: Declaration): List<Diagnostic> {
        visitDeclaration(def)
        return errors
    }
    override fun visitDeclaration(def: Declaration): Unit = when (def) {
        is Declaration.FunctionDef -> {
            visitBlock(def.body)
            def.params.map { visitParam(it) }
            unit
        }
        else -> super.visitDeclaration(def)
    }

    private fun TypeAnnotation.type() = astConv.typeAnnotationToType(this)
    private fun List<TypeAnnotation>.types() = map { it.type() }

}