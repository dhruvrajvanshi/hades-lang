package hadesc.hirgen

import hadesc.analysis.ClosureCaptures
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.defer
import hadesc.hir.*
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.resolver.Binding
import hadesc.scoped
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr
import java.nio.file.Path

internal class HIRGenClosure(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext,
    private val functionContext: HIRGenFunctionContext,
) : HIRGenModuleContext by moduleContext,
    HIRGenFunctionContext by functionContext,
    ASTContext by ctx {
    override val currentModule: HIRModule
        get() = moduleContext.currentModule
    internal fun lowerClosure(expression: Expression.Closure): HIROperand = scoped {
        scopeStack.push(expression)
        defer { check(scopeStack.pop() === expression) }

        return@scoped lowerClosureHelper(expression)
    }

    private fun lowerClosureHelper(expression: Expression.Closure): HIROperand {
        val captureInfo = ctx.analyzer.getClosureCaptures(expression)

        val closureFn = emitClosureFn(expression, captureInfo)
        val closureRef = emit(HIRStatement.AllocateClosure(
            currentLocation,
            ctx.makeUniqueName("closure"),
            expression.type as Type.Function,
            closureFn.ref(),
            emptyList()
        ))
        return closureRef.result()
    }

    private fun emitClosureFn(
        expression: Expression.Closure,
        captureInfo: ClosureCaptures,
    ): HIRDefinition.Function = scoped {
        val fnName = ctx.makeUniqueName("closure_fn")
        val captureParam = HIRParam(
            expression.location,
            Binder(Identifier(currentLocation, closureCtxParamName)),
            Type.Void.ptr(),
        )

        val body = when (expression.body) {
            is ClosureBody.Block -> {
                lowerBlock(expression.body.block)
            }
            is ClosureBody.Expression -> {
                buildBlock {
                    emit(
                        HIRStatement.Return(
                            currentLocation,
                            lowerExpression(expression.body.expression)
                        )
                    )
                }
            }
        }

        val signature = HIRFunctionSignature(
            currentLocation,
            fnName.toQualifiedName(),
            typeParams = captureInfo.types.map { HIRTypeParam(it.location, it.name) }.ifEmpty { null },
            params = expression.params.map {
                HIRParam(it.location, it.binder, ctx.analyzer.getParamType(it))
            } + listOf(
                captureParam
            ),
            returnType = ctx.analyzer.getReturnType(expression)
        )
        val fn = HIRDefinition.Function(
            currentLocation,
            signature,
            mutableListOf(body)
        )
        currentModule.addDefinition(fn)
        fn
    }
    private val closureCtxParamName = ctx.makeName("\$ctx")

    private fun addCaptureStruct(location: SourceLocation, captures: ClosureCaptures): HIRDefinition.Struct {
        val typeParams = captures.types.map { HIRTypeParam(it.location, it.name) }
        val fields = captures.values.entries.map { (name, capture) ->
            val type = capturedFieldType(capture)
            name.name to type
        }
        val structDef = HIRDefinition.Struct(
            name = ctx.makeUniqueName("ClosureContext").toQualifiedName(),
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

    private val closureCtxFieldName = ctx.makeName("ctx")
    private val closureFunctionPtrName = ctx.makeName("fn")
    private val closureStruct by lazy {
        val location = SourceLocation(
            SourcePath(Path.of("builtin.Closure")),
            Position(0, 0),
            Position(0, 0)
        )
        val structName = namingCtx.makeName("\$builtin.Closure")
        val typeParamName = namingCtx.makeName("T")
        val typeParam = Binder(Identifier(location, typeParamName))
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = listOf(HIRTypeParam(location, typeParamName)),
            fields = listOf(
                closureCtxFieldName to Type.Void.ptr(),
                closureFunctionPtrName to fnTypeThatReturns(Type.ParamRef(typeParam)).ptr(),
            ),
            name = structName.toQualifiedName()
        )
        currentModule.addDefinition(def)
        def

    }

    private fun fnTypeThatReturns(returns: Type): Type.Function {
        return Type.Function(
            from = listOf(),
            to = returns
        )
    }
}