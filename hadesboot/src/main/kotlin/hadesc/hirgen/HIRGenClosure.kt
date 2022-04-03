package hadesc.hirgen

import hadesc.analysis.ClosureCaptures
import hadesc.assertions.requireUnreachable
import hadesc.ast.Block
import hadesc.ast.ClosureBody
import hadesc.ast.Expression
import hadesc.ast.Statement
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.defer
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIRParam
import hadesc.hir.HIRTypeParam
import hadesc.location.SourceLocation
import hadesc.resolver.Binding
import hadesc.scoped
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr

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
        val captureStructDef = addCaptureStruct(expression.location, ctx.analyzer.getClosureCaptures(expression))
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

    private fun addCaptureStruct(location: SourceLocation, captures: ClosureCaptures): HIRDefinition.Struct {
        val typeParams = captures.types.map { HIRTypeParam(it.location, it.name) }
        val fields = captures.values.entries.map { (name, capture) ->
            val type = when (capture.first) {
                is Binding.ClosureParam -> capture.second
                is Binding.FunctionParam -> capture.second
                // Local variables are captured by pointer because
                // they can be mutated within the capturing context
                is Binding.ValBinding -> capture.second.mutPtr()
                else -> requireUnreachable {
                    "Illegal closure capture binding: ${capture.first::class}"
                }
            }
            name.name to type
        }
        val structDef = HIRDefinition.Struct(
            name = ctx.makeUniqueName("closure_captures").toQualifiedName(),
            location = location,
            typeParams = typeParams.ifEmpty { null },
            fields = fields
        )
        currentModule.addDefinition(structDef)
        return structDef
    }
}