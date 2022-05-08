package hadesc.hir

import hadesc.Name
import hadesc.assertions.requireUnreachable

private const val CFG_NOT_ALLOWED_ON_UNLOWERED_DEF = "HIRCFGVisitor cannot be called before lowering to basic blocks"
/**
 * Visits a HIRFunction definition in control flow order
 */
interface HIRCFGVisitor: HIRBlockVisitor {
    val functionDef: HIRDefinition.Function
    val visitedBlockSet: MutableSet<Name>
    fun run() {
        functionDef.basicBlocks.firstOrNull()?.let { entryBlock -> visitBlock(entryBlock) }
    }

    override fun visitBlock(block: HIRBlock) {
        if (visitedBlockSet.contains(block.name)) {
            return
        }
        visitedBlockSet.add(block.name)
        super.visitBlock(block)
    }

    override fun visitConditionalBranchStatement(statement: HIRStatement.SwitchInt) {

        super.visitConditionalBranchStatement(statement)
    }

    override fun visitClosure(expression: HIRExpression.Closure) {
        requireUnreachable { CFG_NOT_ALLOWED_ON_UNLOWERED_DEF }
    }

    override fun visitInvokeClosure(expression: HIRExpression.InvokeClosure) {
        requireUnreachable { CFG_NOT_ALLOWED_ON_UNLOWERED_DEF }
    }

    override fun visitWhileStatement(statement: HIRStatement.While) {
        requireUnreachable { CFG_NOT_ALLOWED_ON_UNLOWERED_DEF }
    }

    override fun visitMatchInt(statement: HIRStatement.MatchInt) {
        requireUnreachable { CFG_NOT_ALLOWED_ON_UNLOWERED_DEF }
    }

    override fun visitStatement(statement: HIRStatement) {
        return when (statement) {
            is HIRStatement.NestedControlFlow -> requireUnreachable { CFG_NOT_ALLOWED_ON_UNLOWERED_DEF }
            is HIRStatement.BasicBlockControlFlow ->
                visitBasicBlockControlFlowStatement(statement)
            is HIRStatement.Return,
            is HIRStatement.StraightLineInstruction -> super.visitStatement(statement)
        }
    }

    fun visitBasicBlockControlFlowStatement(statement: HIRStatement.BasicBlockControlFlow): Unit =
        when (statement) {
            is HIRStatement.Jump -> visitBlockWithLabel(statement.to)
            is HIRStatement.SwitchInt -> {
                for (case in statement.cases) {
                    visitBlockWithLabel(case.block)
                }
                visitBlockWithLabel(statement.otherwise)
            }
        }

    fun visitBlockWithLabel(label: Name) {
        functionDef.findBlock(label)
    }
}

class AbstractHIRCFGVisitor(
    override val functionDef: HIRDefinition.Function
): HIRCFGVisitor {
    override val visitedBlockSet: MutableSet<Name> = mutableSetOf()
}