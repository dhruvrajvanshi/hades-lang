package hadesc.hir.analysis

import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.hir.ControlFlowVisitor
import hadesc.hir.HIRBlock

class MissingReturnAnalyzer(private val ctx: Context) : ControlFlowVisitor() {
    override fun visitBlock(block: HIRBlock) {
        super.visitBlock(block)
        if (block.statements.isEmpty()) {
            ctx.diagnosticReporter.report(
                block.location,
                Diagnostic.Kind.MissingReturnValue
            )
        }
    }
}
