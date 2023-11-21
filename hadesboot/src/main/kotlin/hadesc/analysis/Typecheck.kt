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
        Ctx: ResolverCtx,
        // Typecheck assigns types to Expressions and TypeAnnotations
        Ctx: MutableTyCtx
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
                checker.visitDeclaration(declaration)
                checker.errors.forEach {
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
) : SyntaxVisitor where Ctx: ResolverCtx, Ctx: MutableTyCtx {
    val errors = mutableListOf<Diagnostic>()
    private val astConv = ASTConv(
        resolver = ctx.resolver,
        report = { kind, location ->
            errors.add(Diagnostic(location, kind))
        }
    )

    override fun visitType(type: TypeAnnotation) {
        // trigger the side effect of converting it into a type and also reporting errors
        // if required.
        // We don't want to recursively visit type annotations here because
        // astConv already does that.
        // if we call super.visitType here, we'll get duplicate type errors.
        astConv.typeAnnotationToType(type)
    }

    private fun Expression.type() = exprTypes.getOrPut(this) {
        infer.inferExpression(this)
    }
}