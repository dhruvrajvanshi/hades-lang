package hadesc.hir.analysis

import hadesc.Name
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.hir.ControlFlowVisitor
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.location.SourceLocation

class UseAfterMoveAnalyzer(private val ctx: Context): ControlFlowVisitor() {
    private val movedVars = mutableMapOf<Name, SourceLocation>()
    override fun visitMoveStatement(statement: HIRStatement.Move) {
        val moveLocation = movedVars[statement.name]
        if (moveLocation != null) {
            ctx.diagnosticReporter.report(statement.location, Diagnostic.Kind.UseAfterMove(moveLocation))
            return
        }
        movedVars[statement.name] = statement.location
    }

    override fun visitExpression(expression: HIRExpression) {
        when (expression) {
            is HIRExpression.LocalName -> {
                val moveLocation = movedVars[expression.name]

                if (moveLocation != null) {
                    ctx.diagnosticReporter.report(expression.location, Diagnostic.Kind.UseAfterMove(moveLocation))
                }
            }
            else -> super.visitExpression(expression)
        }
    }
}