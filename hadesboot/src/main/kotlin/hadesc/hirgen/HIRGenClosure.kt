package hadesc.hirgen

import hadesc.ast.Block
import hadesc.ast.ClosureBody
import hadesc.ast.Expression
import hadesc.ast.Statement
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.defer
import hadesc.hir.HIRExpression
import hadesc.hir.HIRParam
import hadesc.scoped
internal class HIRGenClosure(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext,
    private val functionContext: HIRGenFunctionContext,
) : HIRGenModuleContext by moduleContext,
    HIRGenFunctionContext by functionContext,
    ASTContext by ctx {
    internal fun lowerClosure(expression: Expression.Closure): HIRExpression = scoped {
        scopeStack.push(expression)
        defer { check(scopeStack.pop() === expression) }
        val params = expression.params.map {
            HIRParam(
                it.location,
                it.binder,
                ctx.analyzer.getParamType(it))
        }
        val header = paramToLocal.declareParamCopies(params)
        val body = when (expression.body) {
            is ClosureBody.Block -> lowerBlock(expression.body.block, header = header)
            is ClosureBody.Expression -> lowerBlock(
                Block(expression.body.location, null,  listOf(
                Block.Member.Statement(
                    Statement.Return(
                        expression.body.location,
                        expression.body.expression
                    )
                )
            )), header = header)
        }
        val captureData = ctx.analyzer.getClosureCaptures(expression)
        val captures = captureData
            .copy(
                values = captureData.values.mapKeys {
                    paramToLocal.fixBinder(it.key)
                }
            )
        HIRExpression.Closure(
            expression.location,
            ctx.analyzer.reduceGenericInstances(expression.type),
            captures,
            params,
            ctx.analyzer.getReturnType(expression),
            body
        )
    }
}