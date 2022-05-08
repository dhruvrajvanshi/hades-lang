package hadesc.hir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.unit

private const val CFG_NOT_ALLOWED_ON_UNLOWERED_DEF = "HIRCFGVisitor cannot be called before lowering to basic blocks"
/**
 * Visits a HIRFunction definition in control flow order
 */
interface HIRCFGVisitor: HIRBlockVisitor {
    val module: HIRModule
    var functionDef: HIRDefinition.Function
    var implementationDef: HIRDefinition.Implementation?
    val visitedBlockSet: MutableSet<Name>
    fun run() {
        module.definitions.forEach { def ->
            visitDefinition(def)
        }
    }

    fun visitDefinition(definition: HIRDefinition) =
        when (definition) {
            is HIRDefinition.Const -> visitConstDefinition(definition)
            is HIRDefinition.ExternConst -> visitExternConstDefinition(definition)
            is HIRDefinition.ExternFunction -> visitExternFunctionDefinition(definition)
            is HIRDefinition.Function -> visitFunctionDefinition(definition)
            is HIRDefinition.Implementation -> visitImplementationDefinition(definition)
            is HIRDefinition.Struct -> visitStructDefinition(definition)
        }

    fun visitStructDefinition(definition: HIRDefinition.Struct) = unit

    fun visitImplementationDefinition(definition: HIRDefinition.Implementation) {
        implementationDef = definition
        for (function in definition.functions) {
            visitFunctionDefinition(function)
        }
        implementationDef = null
    }

    fun visitExternFunctionDefinition(definition: HIRDefinition.ExternFunction) = unit

    fun visitExternConstDefinition(definition: HIRDefinition.ExternConst) = unit

    fun visitConstDefinition(definition: HIRDefinition.Const) {
        visitExpression(definition.initializer)
    }

    fun visitFunctionDefinition(definition: HIRDefinition.Function) {
        functionDef = definition
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
        visitBlock(checkNotNull(functionDef.findBlock(label)))
    }
}

abstract class AbstractHIRCFGVisitor(
    override val module: HIRModule
): HIRCFGVisitor {
    override lateinit var functionDef: HIRDefinition.Function
    override var implementationDef: HIRDefinition.Implementation? = null
    override val visitedBlockSet: MutableSet<Name> = mutableSetOf()
}