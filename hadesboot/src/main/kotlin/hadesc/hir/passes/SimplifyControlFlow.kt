package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.ignore
import hadesc.location.SourceLocation
import hadesc.types.Type
import hadesc.types.ptr
import hadesc.unit
import llvm.makeList

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
            is HIRDefinition.Function -> {
                outputModule.addDefinition(
                    transformFunctionDef(definition)
                )
            }
            is HIRDefinition.Implementation -> visitImplementationDef(definition)
            is HIRDefinition.ExternConst,
            is HIRDefinition.Const,
            is HIRDefinition.ExternFunction,
            is HIRDefinition.Struct -> outputModule.addDefinition(definition)
        }

    private fun visitImplementationDef(definition: HIRDefinition.Implementation) {
        outputModule.addDefinition(definition.copy(
            functions = definition.functions.map { transformFunctionDef(it) }
        ))
    }

    private fun transformFunctionDef(definition: HIRDefinition.Function): HIRDefinition.Function {
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
        currentFunction = oldFn
        return fn
    }

    private fun withinBlock(block: HIRBlock, f: () -> Unit) {
        val oldBlock = currentBlock
        currentBlock = block
        f()
        currentBlock = oldBlock
    }

    private fun lowerBlock(block: HIRBlock) {
        for (statement in block.statements) {
            when (lowerStatement(statement)) {
                StatementControlFlow.EarlyReturn -> break
                StatementControlFlow.NoEarlyReturn -> unit
            }
        }
    }

    sealed interface StatementControlFlow {
        object EarlyReturn: StatementControlFlow
        object NoEarlyReturn: StatementControlFlow
    }
    private fun lowerStatement(statement: HIRStatement): StatementControlFlow =
        when(statement) {
            is HIRStatement.MatchInt -> lowerMatchInt(statement).let { StatementControlFlow.NoEarlyReturn }
            is HIRStatement.While -> lowerWhileStatement(statement).let { StatementControlFlow.NoEarlyReturn }
            is HIRStatement.Return -> {
                appendStatement(statement)
                StatementControlFlow.EarlyReturn
            }
            else -> appendStatement(statement).let { StatementControlFlow.NoEarlyReturn }
        }

    private fun lowerWhileStatement(statement: HIRStatement.While) {
        val startingBlock = checkNotNull(currentBlock)
        val whileEntry = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName("while_entry")))
        val whileBody = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName("while_body")))
        val whileExit = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName("while_exit")))

        appendStatement(goto(statement.conditionBlock.location, whileEntry.name))
        withinBlock(whileEntry) {
            for (s in statement.conditionBlock.statements) {
                when(lowerStatement(s)) {
                    StatementControlFlow.EarlyReturn -> break
                    StatementControlFlow.NoEarlyReturn -> unit
                }
            }
            lowerBlock(statement.conditionBlock)
            val conditionPtr = HIRExpression.LocalRef(statement.conditionBlock.location, Type.Bool.ptr(), statement.conditionName)
            val conditionLoad = appendStatement(HIRStatement.Load(conditionPtr.location, ctx.makeUniqueName(), conditionPtr))
            appendStatement(
                condBr(
                    statement.conditionBlock.location,
                    HIRExpression.LocalRef(conditionLoad.location, Type.Bool, conditionLoad.name),
                    whileBody.name,
                    whileExit.name
                )
            )
        }

        withinBlock(whileBody) {
            var earlyReturn = false
            for (s in statement.body.statements) {
                when(lowerStatement(s)) {
                    StatementControlFlow.EarlyReturn -> {
                        earlyReturn = true
                        break
                    }
                    StatementControlFlow.NoEarlyReturn -> unit
                }
            }
            if (!earlyReturn) {
                appendStatement(
                    goto(statement.body.location, whileEntry.name)
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

    private fun goto(location: SourceLocation, branch: Name): HIRStatement.SwitchInt {
        val trueValue = HIRConstant.IntValue(location, Type.Bool, 1)
        return HIRStatement.SwitchInt(
            location,
            trueValue,
            listOf(
                SwitchIntCase(trueValue, branch)
            ),
            branch
        )
    }

    private fun lowerMatchInt(statement: HIRStatement.MatchInt) {
        val startingBlock = checkNotNull(currentBlock)
        val armBlocks = makeList {
            for (arm in statement.arms) {
                val branch = arm.block
                val branchBlock = appendBasicBlock(HIRBlock(branch.location, ctx.makeUniqueName()))
                withinBlock(branchBlock) {
                    lowerBlock(branch)
                }
                add(arm to branchBlock)
            }
        }
        val otherwise = appendBasicBlock(HIRBlock(statement.otherwise.location, ctx.makeUniqueName()))
        withinBlock(otherwise) {
            lowerBlock(statement.otherwise)
        }

        val end = appendBasicBlock(HIRBlock(statement.location, ctx.makeUniqueName()))

        val otherwiseBranchName = checkNotNull(otherwise.name)
        val endBranchName = checkNotNull(end.name)

        startingBlock.statements.add(
            HIRStatement.SwitchInt(
                statement.value.location,
                statement.value,
                armBlocks.map { (arm, armBlock) ->
                    val armBlockName = checkNotNull(armBlock.name)
                    SwitchIntCase(
                        arm.value,
                        armBlockName
                    )
                },
                otherwiseBranchName
            )
        )

        for ((arm, armBlock) in armBlocks) {
            terminateBlock(armBlock) {
                goto(arm.block.location, endBranchName)
            }
        }
        terminateBlock(otherwise) {
            goto(statement.otherwise.location, endBranchName)
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
            is HIRStatement.Store,
            is HIRStatement.Alloca,
            is HIRStatement.Call,
            is HIRStatement.Load,
            is HIRStatement.GetStructField,
            is HIRStatement.GetStructFieldPointer,
            is HIRStatement.Not,
            is HIRStatement.IntegerConvert,
            is HIRStatement.TypeApplication,
            is HIRStatement.PointerCast,
            is HIRStatement.BinOp,
            is HIRStatement.AllocateClosure,
            is HIRStatement.InvokeClosure,
            is HIRStatement.Move,
            -> false

            is HIRStatement.Return,
            is HIRStatement.Jump,
            is HIRStatement.SwitchInt,
                -> true
            is HIRStatement.MatchInt -> requireUnreachable()
            is HIRStatement.While -> requireUnreachable()
        }
    }

    private fun appendBasicBlock(block: HIRBlock): HIRBlock {
        checkNotNull(currentFunction).basicBlocks.add(block)
        return block
    }

    private fun <T: HIRStatement> appendStatement(statement: T, intoBlock: HIRBlock = checkNotNull(currentBlock)): T {
        intoBlock.statements.add(statement)
        return statement
    }

    private fun getBlock(name: Name): HIRBlock {
        return checkNotNull(
            checkNotNull(currentFunction).basicBlocks.find {
                it.name == name
            }
        )
    }
}