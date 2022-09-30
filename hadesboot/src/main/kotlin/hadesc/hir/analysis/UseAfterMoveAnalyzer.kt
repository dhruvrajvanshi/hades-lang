package hadesc.hir.analysis

import hadesc.Name
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.hir.ControlFlowVisitor
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement

class UseAfterMoveAnalyzer(private val ctx: Context): ControlFlowVisitor() {
    private val movedVarSet = mutableSetOf<Name>()
    override fun visitMoveStatement(statement: HIRStatement.Move) {
        if (isMoved(statement.name)) {
            ctx.diagnosticReporter.report(statement.location, Diagnostic.Kind.UseAfterMove)
            return
        }
        movedVarSet.add(statement.name)
    }

    override fun visitExpression(expression: HIRExpression) {
        when (expression) {
            is HIRExpression.LocalName -> {
                if (isMoved(expression.name)) {
                    ctx.diagnosticReporter.report(expression.location, Diagnostic.Kind.UseAfterMove)
                }
            }
            else -> super.visitExpression(expression)
        }
    }

    private fun isMoved(name: Name): Boolean {
        return name in movedVarSet
    }
}