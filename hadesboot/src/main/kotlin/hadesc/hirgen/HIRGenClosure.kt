package hadesc.hirgen

import hadesc.Name
import hadesc.analysis.ClosureCaptures
import hadesc.ast.*
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
import libhades.collections.Stack

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
        val captureStruct = emitCaptureStruct(expression.location, captureInfo)

        val closureFn = emitClosureFn(expression, captureInfo, captureStruct)

        val captureTypeArgs =
            if (captureStruct.typeParams == null)
                emptyList()
            else
                captureInfo.types.map { Type.ParamRef(it) }
        val contextRef = emitAlloca("closureCtxPtr", captureStruct.instanceType(captureTypeArgs))

        emitCaptureInitializers(contextRef, captureInfo)

        val fnRef = if (captureInfo.types.isEmpty())
                        closureFn.ref()
                    else
                        emitTypeApplication(closureFn.ref(), captureTypeArgs).result()

        val closureRef = emit(HIRStatement.AllocateClosure(
            currentLocation,
            ctx.makeUniqueName("closure"),
            ctx.analyzer.reduceGenericInstances(expression.type) as Type.Function,
            fnRef,
            contextRef.ptr()
        ))
        return closureRef.result()
    }

    private fun emitCaptureInitializers(contextRef: HIRStatement.Alloca, captureInfo: ClosureCaptures) {
        for ((binder, pair) in captureInfo.values) {
            val (binding, type) = pair

            when (binding) {
                is Binding.ClosureParam,
                is Binding.FunctionParam -> {
                    emitStore(
                        contextRef.ptr().fieldPtr(binder.name),
                        HIRExpression.ParamRef(
                            currentLocation,
                            type,
                            binding.binder.name,
                            binding.binder
                        )
                    )
                }
                is Binding.ValBinding -> {
                    val capturePtr = getCapturePointer(binding)
                    emitStore(
                        contextRef.ptr()
                            .fieldPtr(binder.name, ctx.makeUniqueName(binder.name.text + "_ptr_ptr")),
                        capturePtr
                    )
                }
                is Binding.MatchArmEnumCaseArg -> {
                    val capturePtr = getCapturePointer(binding)
                    emitStore(
                        contextRef.ptr()
                            .fieldPtr(binder.name, ctx.makeUniqueName(binder.name.text + "_ptr_ptr")),
                        capturePtr
                    )
                }
            }
        }
    }

    private fun emitCaptureStruct(location: SourceLocation, captureInfo: ClosureCaptures): HIRDefinition.Struct {
        return emitDef(
            HIRDefinition.Struct(
                location = location,
                name = namingCtx.makeUniqueName("ClosureCaptures").toQualifiedName(),
                typeParams = captureInfo.types.map { HIRTypeParam(it.location, it.name) },
                fields = captureInfo.values.map {
                    val captureTy = it.value.second
                    val capturedBinding = it.value.first
                    val fieldTy =
                        if (isCapturedByValue(capturedBinding))
                            captureTy
                        else
                            captureTy.mutPtr()
                    it.key.name to fieldTy
                }
            )
        )
    }

    private fun isCapturedByValue(capturedBinding: Binding.Local): Boolean {
        return when (capturedBinding) {
            is Binding.ClosureParam,
            is Binding.FunctionParam -> true
            is Binding.ValBinding -> false
            is Binding.MatchArmEnumCaseArg -> false
        }
    }

    private val closureGenStack = Stack<ClosureGenContext>()
    private fun emitClosureFn(
        expression: Expression.Closure,
        captureInfo: ClosureCaptures,
        captureStruct: HIRDefinition.Struct
    ): HIRDefinition.Function = scoped {
        val fnName = ctx.makeUniqueName("closure_fn")
        val captureTypeArgs =
            if (captureStruct.typeParams == null)
                emptyList()
            else
                captureInfo.types.map { Type.ParamRef(it) }

        val captureParam = HIRParam(
            expression.location,
            Binder(Identifier(currentLocation, closureCtxParamName)),
            captureStruct.instanceType(captureTypeArgs).ptr(),
        )
        val returnType = ctx.analyzer.getReturnType(expression)
        val body = scoped {
            closureGenStack.push(ClosureGenContext(captureParam, captureStruct))
            defer { closureGenStack.pop() }
            val intoBlock = HIRBlock(expression.body.location, ctx.makeName("entry"))
            when (expression.body) {
                is ClosureBody.Block -> {
                    val addReturnVoid = returnType is Type.Void && !hasTerminator(expression.body.block)
                    lowerBlock(expression.body.block, addReturnVoid = addReturnVoid, into=intoBlock)
                }
                is ClosureBody.Expression -> {
                    buildBlock(into=intoBlock) {
                        emit(
                            HIRStatement.Return(
                                currentLocation,
                                lowerExpression(expression.body.expression)
                            )
                        )
                    }
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
            returnType = returnType
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

    private fun Identifier.uniqueNameWithSuffix(suffix: String): Name {
        return namingCtx.makeUniqueName(name.text + suffix)
    }
    internal fun lowerCaptureBinding(name: Identifier, binding: Binding.Local): HIROperand {
        val closureCtx = closureGenStack.peek()
        check(closureCtx != null)
        val captureParam = closureCtx.captureParam
        return when (binding) {
            // Function and Closure params are captured by value
            // ValBindings are captured by ptr (because they can be mutated)
            is Binding.ClosureParam,
            is Binding.FunctionParam ->
                captureParam.ref() // *Ctx
                    .fieldPtr(
                        name.name,
                        name.uniqueNameWithSuffix("_ptr")
                    ) // *FieldType
                    .load(name.uniqueNameWithSuffix("_val")) // FieldType
            is Binding.ValBinding ->
                getCapturePointer(binding)
                    .load(name.uniqueNameWithSuffix("_val")) // FieldType
            is Binding.MatchArmEnumCaseArg ->
                getCapturePointer(binding)
                    .load(name.uniqueNameWithSuffix("_val")) // FieldType
        }
    }

    internal fun lowerCaptureAssignment(statement: Statement.LocalAssignment) {
        val closureCtx = closureGenStack.peek()
        check(closureCtx != null)
        val ptr =
            closureCtx.captureParam.ref() // *Ctx
                .fieldPtr(statement.name.name) // **mut FieldType
                .load() // *mut FieldType
        emitStore(ptr, lowerExpression(statement.value))
    }

    private fun getCapturePointer(binding: Binding.ValBinding): HIROperand =
        getCapturePointer(binding.binder)
    private fun getCapturePointer(binding: Binding.MatchArmEnumCaseArg): HIROperand =
        getCapturePointer(binding.binder)

    private fun getCapturePointer(binder: Binder): HIROperand {
        for (closureGenCtx in closureGenStack.items().reversed()) {
            closureGenCtx.getCapture(binder.name) ?: continue
            val ctxPtr = closureGenCtx.captureParam.ref()
            return ctxPtr.fieldPtr(binder.name)
                .load()
        }
        return HIRExpression.LocalRef(
            currentLocation,
            ctx.analyzer.typeOfBinder(binder).mutPtr(),
            binder.name
        )
    }
}

data class ClosureGenContext(
    val captureParam: HIRParam,
    val captureStruct: HIRDefinition.Struct,
) {
    fun getCapture(name: Name): Type? {
        return captureStruct.fields.find { it.first == name }?.second
    }
}