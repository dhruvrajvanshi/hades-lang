package hadesc.analysis

import hadesc.ast.*
import hadesc.context.DiagnosticReporterCtx
import hadesc.context.ResolverCtx
import hadesc.context.SourceFileResolverCtx
import hadesc.diagnostics.Diagnostic
import hadesc.diagnostics.DiagnosticReporter
import hadesc.types.Type
import hadesc.unit

class Typecheck<Ctx>(
    private val ctx: Ctx,
) where Ctx: SourceFileResolverCtx,
        Ctx: DiagnosticReporterCtx,
        Ctx: ResolverCtx
{
    private val exprTypes = MutableNodeMap<Expression, Type>()
    private val annotationTypes = MutableNodeMap<TypeAnnotation, Type>()
    fun typecheck() {
        ctx.forEachSourceFile { sourceFile ->
            for (declaration in sourceFile.declarations) {
                val infer = Infer.make()
                val checker = TypecheckImpl(
                    ctx,
                    infer,
                    ctx.diagnosticReporter,
                    exprTypes,
                    annotationTypes,
                )
                val errors = checker(declaration)
                errors.forEach {
                    ctx.diagnosticReporter.report(it.sourceLocation, it.kind)
                }
            }
        }
    }
}

/**
 * Typechecker for a single declaration.
 * A new one must be created for each declaration because it creates
 * a new inference context.
 */
private class TypecheckImpl<Ctx>(
    private val ctx: Ctx,
    private val infer: Infer,
    private val diagnostic: DiagnosticReporter,
    /**
     * out parameter to put types into
     */
    private val exprTypes: MutableNodeMap<Expression, Type>,
    private val annotationTypes: MutableNodeMap<TypeAnnotation, Type>,
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

    private fun TypeAnnotation.type() = annotationTypes.getOrPut(this) {
        astConv.typeAnnotationToType(this)
    }
    private fun Expression.type() = exprTypes.getOrPut(this) {
        infer.inferExpression(this)
    }
    private fun List<TypeAnnotation>.types() = map { it.type() }

}