package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.location.SourceLocation
import hadesc.types.Type

/**
 * Converts all structured control flow (if/else, while, etc)
 * into simple branch and conditional branches.
 *
 * All blocks are converted into basic blocks (straight line
 * statement sequences that don't branch in the middle).
 *
 * This flattens out nested blocks, making it easier to generate
 * instructions (LLVM or otherwise)
 */
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
            basicBlocks = mutableListOf()
        )
        currentFunction = fn

        for (block in definition.basicBlocks) {
            val newBlock = appendBasicBlock(HIRBlock(block.location, name = block.name))
            withinBlock(newBlock) {
                lowerBlock(block)
            }
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
        val whileBody = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName()))
        val whileExit = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName()))

        withinBlock(startingBlock)  {
            appendStatement(
                condBr(
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
                condBr(
                    statement.condition.location,
                    statement.condition,
                    checkNotNull(whileBody.name),
                    checkNotNull(whileExit.name),
                )
            }
        }
        currentBlock = whileExit
    }

    private fun condBr(location: SourceLocation, condition: HIRExpression, trueBranch: Name, falseBranch: Name): HIRStatement.SwitchInt {
        return HIRStatement.SwitchInt(
            location,
            condition,
            listOf(
                SwitchIntCase(
                    HIRConstant.IntValue(location, Type.Bool, 1),
                    trueBranch
                )
            ),
            falseBranch
        )
    }

    private fun lowerIfStatement(statement: HIRStatement.If) {
        val startingBlock = checkNotNull(currentBlock)
        val ifTrue = appendBasicBlock(HIRBlock(statement.trueBranch.location, ctx.makeUniqueName()))
        withinBlock(ifTrue) {
            lowerBlock(statement.trueBranch)
        }
        val ifFalse = appendBasicBlock(HIRBlock(statement.falseBranch.location, ctx.makeUniqueName()))
        withinBlock(ifFalse) {
            lowerBlock(statement.falseBranch)
        }

        val end = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName()))

        val trueBranchName = checkNotNull(ifTrue.name)
        val falseBranchName = checkNotNull(ifFalse.name)
        val endBranchName = checkNotNull(end.name)

        startingBlock.statements.add(
            condBr(
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
                    is HIRStatement.SwitchInt -> {
                        for (case in statement.cases) {
                            visitBlock(getBlock(case.block))
                        }
                        visitBlock(getBlock(statement.otherwise))
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
            is HIRStatement.SwitchInt,
            is HIRStatement.ReturnVoid,
            is HIRStatement.Branch -> true

            is HIRStatement.If -> requireUnreachable()
            is HIRStatement.While -> requireUnreachable()
        }
    }

    private fun appendBasicBlock(block: HIRBlock): HIRBlock {
        checkNotNull(currentFunction).basicBlocks.add(block)
        return block
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