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

    internal fun lowerClosure(expression: Expression.Closure): HIRExpression = scoped {
        scopeStack.push(expression)
        defer { check(scopeStack.pop() === expression) }
        val params = expression.params.map {
            HIRParam(
                it.location,
                it.binder,
                ctx.analyzer.getParamType(it))
        }

        lowerClosureNew(expression)

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

    private val enableNewClosureGen = false
    private fun lowerClosureNew(expression: Expression.Closure) {
        if (!enableNewClosureGen) return
        val captureInfo = ctx.analyzer.getClosureCaptures(expression)
        val ctxStruct = addCaptureStruct(expression.location, captureInfo)
        val ctxTypeArgs = captureInfo.types.map { Type.ParamRef(it) }
        val ctxRef = emitAlloca(ctx.makeUniqueName("closure_ctx"), ctxStruct.instanceType(ctxTypeArgs))
        emitCaptureFieldInitializers(captureInfo, ctxRef)

        val closureFn = emitClosureFn(expression, captureInfo, ctxStruct)
        val returnType = ctx.analyzer.getReturnType(expression)
    }

    private fun emitClosureFn(
        expression: Expression.Closure,
        captureInfo: ClosureCaptures,
        captureStructDef: HIRDefinition.Struct,
    ): HIRDefinition.Function = scoped {
        val fnName = ctx.makeUniqueName("closure_fn")
        val captureParam = HIRParam(
            expression.location,
            Binder(Identifier(currentLocation, closureCtxParamName)),
            captureStructDef.instanceType(
                captureInfo.types.map { Type.ParamRef(it) }
            ).ptr()
        )

        val oldSubstitution = valueSubstitution
        val oldAssignmentSubstitution = localAssignmentSubstitution
        val newSubstitution: ValueSubstitution = mutableMapOf()
        val newAssignmentSubstitution: ValueSubstitution = mutableMapOf()
        captureInfo.values.entries.forEachIndexed { index, (binder, capture) ->
            val type = capture.second
            val binding = capture.first
            val isCapturedByPointer = binding is Binding.ValBinding
            if (isCapturedByPointer) {
                newSubstitution[binder] = {
                    HIRExpression.LocalRef(
                        currentLocation,
                        type.mutPtr(),
                        binder.name
                    ).load()
                }
                newAssignmentSubstitution[binder] = {
                    HIRExpression.LocalRef(
                        currentLocation,
                        type.mutPtr(),
                        binder.name
                    )
                }
            } else {
                newSubstitution[binder] = {
                    HIRExpression.LocalRef(
                        currentLocation,
                        type,
                        binder.name
                    )
                }
            }
        }
        valueSubstitution = newSubstitution
        defer {
            check(valueSubstitution === newSubstitution)
            valueSubstitution = oldSubstitution
        }

        localAssignmentSubstitution = newAssignmentSubstitution
        defer {
            check(localAssignmentSubstitution === newAssignmentSubstitution)
            localAssignmentSubstitution = oldAssignmentSubstitution
        }


        val body = when (expression.body) {
            is ClosureBody.Block -> {
                lowerBlock(expression.body.block, before = {
                    emitClosureFnHeader(captureInfo, captureParam)
                })
            }
            is ClosureBody.Expression -> {
                buildBlock {
                    emitClosureFnHeader(captureInfo, captureParam)
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

    private fun emitClosureFnHeader(captures: ClosureCaptures, capturePtrParam: HIRParam) {
        captures.values.entries.forEachIndexed { index, (binder, capture) ->
            val type = capture.second
            val binding = capture.first
            val capturePtr = capturePtrParam.ref()
            val isCapturedByPointer = binding is Binding.ValBinding

            if (isCapturedByPointer) {
                capturePtr
                    .fieldPtr(binder.name, index, type.mutPtr().ptr())
                    .load(binder.name)
            } else {
                capturePtr
                    .fieldPtr(binder.name, index, type.ptr()) // * capture.type
                    .load(binder.name)
            }
        }
    }

    private val closureCtxParamName = ctx.makeName("\$ctx")

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
            file = SourcePath(Path.of("builtin.Closure")),
            start = Position(0, 0),
            stop = Position(0, 0)
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