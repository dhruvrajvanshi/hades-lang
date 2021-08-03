package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.logging.logger

class SimplifyControlFlow(private val ctx: Context) {
    private val outputModule = HIRModule(mutableListOf())
    private var currentFunction: HIRDefinition.Function? = null
    private var currentBlock: HIRBlock? = null

    fun transformModule(module: HIRModule): HIRModule {
        for (definition in module.definitions) {
            visitDefinition(definition)
        }
        return outputModule
    }

    private fun visitDefinition(definition: HIRDefinition) =
        when (definition) {
            is HIRDefinition.Function -> visitFunctionDef(definition)
            is HIRDefinition.Implementation -> requireUnreachable()
            is HIRDefinition.ExternConst,
            is HIRDefinition.Const,
            is HIRDefinition.ExternFunction,
            is HIRDefinition.Struct -> outputModule.addDefinition(definition)
        }

    private fun visitFunctionDef(definition: HIRDefinition.Function) {
        val oldFn = currentFunction
        val fn = HIRDefinition.Function(
            definition.location,
            signature = definition.signature,
            body = HIRBlock(definition.location),
            basicBlocks = mutableListOf()
        )

        currentFunction = fn
        val entryBlock = HIRBlock(definition.body.location, mutableListOf(), ctx.makeName("entry"))
        fn.basicBlocks.add(entryBlock)
        withinBlock(entryBlock) {
            lowerBlock(definition.body)
        }
        outputModule.addDefinition(fn)
        currentFunction = oldFn
    }

    private fun withinBlock(block: HIRBlock, f: () -> Unit) {
        val oldBlock = currentBlock
        currentBlock = block
        f()
        currentBlock = oldBlock
    }

    private fun lowerBlock(block: HIRBlock) {
        for (statement in block.statements) {
            lowerStatement(statement)
        }
    }

    private fun lowerStatement(statement: HIRStatement) =
        when(statement) {
            is HIRStatement.If -> lowerIfStatement(statement)
            is HIRStatement.While -> lowerWhileStatement(statement)
            else -> appendStatement(statement)
        }

    private fun lowerWhileStatement(statement: HIRStatement.While) {
        val startingBlock = checkNotNull(currentBlock)
        val whileBody = appendBasicBlock(HIRBlock(statement.location))
        val whileExit = appendBasicBlock(HIRBlock(statement.location))

        withinBlock(startingBlock)  {
            appendStatement(
                HIRStatement.ConditionalBranch(
                    statement.condition.location,
                    statement.condition,
                    checkNotNull(whileBody.name),
                    checkNotNull(whileExit.name),
                )
            )
        }

        withinBlock(whileBody) {
            lowerBlock(statement.body)
            terminateBlock(whileBody) {
                HIRStatement.ConditionalBranch(
                    statement.condition.location,
                    statement.condition,
                    checkNotNull(whileBody.name),
                    checkNotNull(whileExit.name),
                )
            }
        }
        currentBlock = whileExit
    }

    private fun lowerIfStatement(statement: HIRStatement.If) {
        val startingBlock = checkNotNull(currentBlock)
        val ifTrue = appendBasicBlock(HIRBlock(statement.trueBranch.location))
        withinBlock(ifTrue) {
            lowerBlock(statement.trueBranch)
        }
        val ifFalse = appendBasicBlock(HIRBlock(statement.falseBranch.location))
        withinBlock(ifFalse) {
            lowerBlock(statement.falseBranch)
        }

        val end = appendBasicBlock(HIRBlock(statement.location))

        val trueBranchName = checkNotNull(ifTrue.name)
        val falseBranchName = checkNotNull(ifFalse.name)
        val endBranchName = checkNotNull(end.name)

        startingBlock.statements.add(
            HIRStatement.ConditionalBranch(
                statement.condition.location,
                statement.condition,
                trueBranchName,
                falseBranchName
            )
        )

        terminateBlock(ifTrue) {
            HIRStatement.Branch(statement.trueBranch.location, endBranchName)
        }
        terminateBlock(ifFalse) {
            HIRStatement.Branch(statement.falseBranch.location, endBranchName)
        }

        currentBlock = end
    }

    private fun terminateBlock(entryBlock: HIRBlock, f: () -> HIRStatement) {
        val isVisited = mutableSetOf<Name>()
        fun visitBlock(branch: HIRBlock) {
            checkNotNull(branch.name)
            if (isVisited.contains(branch.name)) {
                return
            }
            isVisited.add(branch.name)
            if (!branch.hasTerminator()) {
                withinBlock(branch) {
                    appendStatement(f())
                }
            } else {
                when (val statement = branch.statements.last()) {
                    is HIRStatement.Return -> Unit
                    is HIRStatement.ReturnVoid -> Unit
                    is HIRStatement.Branch -> {
                        val block1 = getBlock(statement.toBranchName)
                        visitBlock(block1)
                    }
                    is HIRStatement.ConditionalBranch -> {
                        visitBlock(getBlock(statement.trueBranchName))
                        visitBlock(getBlock(statement.falseBranchName))
                    }
                    else -> Unit
                }
            }
        }
        visitBlock(entryBlock)
    }

    private fun HIRBlock.hasTerminator(): Boolean {
        val last = statements.lastOrNull() ?: return false

        return when (last) {
            is HIRStatement.Assignment,
            is HIRStatement.Store,
            is HIRStatement.ValDeclaration,
            is HIRStatement.Expression, -> false

            is HIRStatement.Return,
            is HIRStatement.ConditionalBranch,
            is HIRStatement.ReturnVoid,
            is HIRStatement.Branch -> true

            is HIRStatement.If -> requireUnreachable()
            is HIRStatement.While -> requireUnreachable()
        }
    }

    private fun appendBasicBlock(block: HIRBlock): HIRBlock {
        val result = if (block.name == null) block.copy(name = ctx.makeUniqueName()) else block
        checkNotNull(currentFunction).basicBlocks.add(result)
        return result
    }

    private fun appendStatement(statement: HIRStatement, intoBlock: HIRBlock = checkNotNull(currentBlock)) {
        intoBlock.statements.add(statement)
    }

    private fun getBlock(name: Name): HIRBlock {
        return checkNotNull(
            checkNotNull(currentFunction).basicBlocks.find {
                it.name == name
            }
        )
    }
}