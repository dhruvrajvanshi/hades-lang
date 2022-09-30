package hadesc.hir

import hadesc.*

/**
 * Base visitor class that visits each function in the module,
 * visiting its basic blocks in control flow order.
 * Ensures each basic block is visited only once.
 */
abstract class ControlFlowVisitor: HIRModuleVisitor {
    private val workList = ArrayDeque<HIRBlock>()
    private var visitedBlockSet: MutableSet<Name>? = null
    private var currentFunction: HIRDefinition.Function? = null

    override fun visitFunctionDef(definition: HIRDefinition.Function) = scoped {
        check(definition.basicBlocks.isNotEmpty())

        check(currentFunction == null)
        currentFunction = definition
        defer { check(currentFunction === definition) }
        defer { currentFunction = null }

        check(visitedBlockSet == null)
        visitedBlockSet = mutableSetOf()
        defer { visitedBlockSet = null }

        workList.add(definition.basicBlocks.first())
        while (workList.isNotEmpty()) {
            val block = workList.removeFirst()
            val visitedSet = checkNotNull(visitedBlockSet)
            if (block.name in visitedSet) {
                continue
            }

            checkNotNull(visitedBlockSet).add(block.name)
            visitBlock(block)

            check(block.statements.isNotEmpty())
            val terminator = block.statements.last()
            check(terminator is HIRStatement.Terminator) {
                "Unterminated block found(${block.name.text}): ${block.location}"
            }

            when (terminator) {
                is HIRStatement.Jump ->
                    workList.add(definition.findBlockOrThrow(terminator.to))
                is HIRStatement.SwitchInt -> {
                    for (case in terminator.cases) {
                        workList.add(definition.findBlockOrThrow(case.block))
                    }
                    workList.add(definition.findBlockOrThrow(terminator.otherwise))
                }
                is HIRStatement.Return -> {}
            }

        }

        unit
    }

    override fun visitBlock(block: HIRBlock) {

        val isBasicBlock = block.statements.dropLast().all {
            it is HIRStatement.StraightLineInstruction
        }
        check(isBasicBlock)
        for (statement in block.statements) {
            check(statement !is HIRStatement.NestedControlFlow) {
                "Found an unexpected nested control flow statement" +
                        "ControlFlowVisitor must only be called after SimplifyControlFlow has been run"
            }
            visitStatement(statement)
        }
    }
}