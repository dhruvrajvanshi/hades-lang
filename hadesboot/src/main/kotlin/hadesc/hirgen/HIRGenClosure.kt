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
import hadesc.hir.*
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
        val captureInfo = ctx.analyzer.getClosureCaptures(expression)
        val captureStructDef = addCaptureStruct(expression.location, captureInfo)
        val captureTypeArgs = captureInfo.types.map { Type.ParamRef(it) }
        val captureRef = emitAlloca(ctx.makeUniqueName("closure_capture"), captureStructDef.instanceType(captureTypeArgs))
        emitCaptureFieldInitializers(captureInfo, captureRef)

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

    private fun emitCaptureFieldInitializers(captureInfo: ClosureCaptures, captureRef: HIRStatement.Alloca) {
        captureInfo.values.entries.forEachIndexed { index, (binder, capture) ->
            val type = capturedFieldType(capture)
            val fieldPtr = captureRef.mutPtr().fieldPtr(binder.name, index, type.mutPtr())

            val value = when (capture.first) {
                is Binding.ValBinding -> {
                    HIRExpression.LocalRef(
                        currentLocation,
                        type,
                        binder.name
                    ).load()
                }
                is Binding.FunctionParam -> {
                    HIRExpression.ParamRef(
                        currentLocation,
                        type,
                        binder.name,
                        binder,
                    )
                }
                is Binding.ClosureParam -> requireUnreachable()
            }
            emitStore(fieldPtr, value)
        }
    }

    private fun addCaptureStruct(location: SourceLocation, captures: ClosureCaptures): HIRDefinition.Struct {
        val typeParams = captures.types.map { HIRTypeParam(it.location, it.name) }
        val fields = captures.values.entries.map { (name, capture) ->
            val type = capturedFieldType(capture)
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

    private fun capturedFieldType(capture: Pair<Binding.Local, Type>): Type {
        return when (capture.first) {
            is Binding.ClosureParam -> capture.second
            is Binding.FunctionParam -> capture.second
            // Local variables are captured by pointer because
            // they can be mutated within the capturing context
            is Binding.ValBinding -> capture.second.mutPtr()
        }
    }
}