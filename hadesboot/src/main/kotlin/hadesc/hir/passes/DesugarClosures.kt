package hadesc.hir.passes

import hadesc.Name
import hadesc.analysis.ClosureCaptures
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.hir.HIRExpression.*
import hadesc.hir.HIRStatement.*
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.types.Type
import hadesc.types.ptr
import libhades.collections.Stack
import java.nio.file.Path

class DesugarClosures(override val namingCtx: NamingContext): AbstractHIRTransformer() {
    private val definitions = mutableListOf<HIRDefinition>()

    private val closureCtxFieldName = namingCtx.makeName("ctx")
    private val closureFunctionPtrName = namingCtx.makeName("functionPtr")
    private val closureCtxFieldIndex = 0
    private val closureFuncPtrFieldIndex = 1
    private val captureStack = Stack<Map<Name, CaptureInfo>>()

    private val structDefForClosureWithResult by lazy {
        val location = SourceLocation(
            file = SourcePath(Path.of("builtin.ClosureWithResult")),
            start = Position(0, 0),
            stop = Position(0, 0)
        )
        val structName = namingCtx.makeName("\$builtin.ClosureWithResult")
        val typeParamName = namingCtx.makeName("T")
        val typeParam = Binder(Identifier(location, typeParamName))
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = listOf(HIRTypeParam(location, typeParamName)),
            fields = listOf(
                closureCtxFieldName to Type.Void.ptr(),
                closureFunctionPtrName to
                        Type.Function(
                            from = emptyList(),
                            traitRequirements = emptyList(),
                            to = Type.ParamRef(typeParam)).ptr()
            ),
            name = structName.toQualifiedName()
        )
        definitions.add(def)
        def

    }
    private val structDefForClosureWithoutResult by lazy {
        val location = SourceLocation(
            file = SourcePath(Path.of("builtin.ClosureWithoutResult")),
            start = Position(0, 0),
            stop = Position(0, 0)
        )
        val structName = namingCtx.makeName("\$builtin.ClosureWithoutResult")
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = null,
            fields = listOf(
                closureCtxFieldName to Type.Ptr(Type.Void, isMutable = true),
                closureFunctionPtrName to
                        Type.Ptr(Type.Function(
                            from = listOf(), traitRequirements = emptyList(), to = Type.Void), isMutable = false)
            ),
            name = structName.toQualifiedName()
        )
        definitions.add(def)
        def

    }

    override fun transformModule(oldModule: HIRModule): HIRModule {
        logger().debug("Before closure desugaring: ${oldModule.prettyPrint()}")
        for (definition in oldModule.definitions) {
            definitions.addAll(transformDefinition(definition))
        }
        return HIRModule(definitions)
    }

    override fun transformInvokeClosure(expression: InvokeClosure): HIRExpression {
        val closureRef = transformExpression(expression.closure)
        return Call(
            expression.location,
            expression.type,
            GetStructField(
                closureRef.location,
                type = Type.Function(from = listOf(Type.Void.ptr()), to = expression.type, traitRequirements = null).ptr(),
                lhs = closureRef,
                name = closureFunctionPtrName,
                index = closureFuncPtrFieldIndex
            ).ptrCast(Type.Function(
                from = expression.args.map { it.type } + listOf(Type.Void.ptr()),
                to = expression.type,
                traitRequirements = null
            )),
            expression.args.map { transformExpression(it) } + GetStructField(
                closureRef.location,
                type = Type.Void.ptr(),
                lhs = closureRef,
                name = closureCtxFieldName,
                index = closureCtxFieldIndex,
            )
        )
    }

    override fun transformAssignmentStatement(statement: Assignment): Collection<HIRStatement> {
        val capture = findCapture(statement.name) ?: return super.transformAssignmentStatement(statement)
        val ptr = getCapturedVariablePointer(statement.location, statement.value.type, statement.name, capture)
        return listOf(Store(
            statement.location,
            ptr,
            transformExpression(statement.value)
        ))
    }

    private fun findCapture(name: Name): CaptureInfo? {
        for (captures in captureStack) {
            val capture = captures[name]
            if (capture != null) {
                return capture
            }
        }
        return null
    }

    override fun transformValRef(expression: ValRef): HIRExpression {
        val capture = findCapture(expression.name) ?: return super.transformValRef(expression)
        return Load(
            expression.location,
            expression.type,
            getCapturedVariablePointer(expression.location, expression.type, expression.name, capture)
        )
    }

    private fun getCapturedVariablePointer(
        location: SourceLocation,
        type: Type,
        varName: Name,
        captureInfo: CaptureInfo): HIRExpression {
        return GetStructField(
            location,
            Type.Ptr(type, isMutable = true),
            lhs = ValRef(
                location,
                captureInfo.contextType,
                captureInfo.contextName
            ),
            name = varName,
            index = captureInfo.index
        )
    }

    override fun transformValDeclaration(statement: Alloca): Collection<HIRStatement> {
        if (statement.type is Type.Function) {
            return listOf(
                Alloca(
                    statement.location,
                    statement.name,
                    statement.isMutable,
                    getClosureType(statement.type)
                )
            )
        }
        return super.transformValDeclaration(statement)
    }

    override fun transformClosure(expression: Closure): HIRExpression {
        val type = expression.type
        require(type is Type.Function)

        val contextStruct = makeAndAddClosureContextStruct(expression.location, expression.captures)
        val contextTypeConstructor = Type.Constructor(contextStruct.name)
        val capturedTypeArgs = expression.captures.types.map { Type.ParamRef(Binder(Identifier(it.location, it.name))) }
        val contextType =
            if (expression.captures.types.isEmpty())
                contextTypeConstructor
            else
                Type.Application(contextTypeConstructor, capturedTypeArgs)

        val contextName = namingCtx.makeUniqueName()
        val contextParamName = namingCtx.makeName("\$ctx")
        val contextDerefname = namingCtx.makeName("\$ctx\$deref")

        val closureName = namingCtx.makeUniqueName()
        val closureType = getClosureType(type)

        // val contextName: contextType
        declareVariable(contextName, contextType)

        // val closureName: closureType
        declareVariable(closureName, closureType)

        val pointersToCaptures = expression.captures.values.map {
            AddressOf(
                expression.location,
                Type.Ptr(it.value, isMutable = true),
                it.key.name
            )
        }

        val contextConstructorRef = GlobalRef(
            expression.location,
            contextStruct.constructorType,
            contextStruct.name
        )
        val contextConstructorCallee =
            if (expression.captures.types.isEmpty())
                contextConstructorRef
            else
                TypeApplication(
                    expression.location,
                    contextConstructorRef.type, // FIXME: This isn't the correct type of this expression.
                    contextConstructorRef,
                    capturedTypeArgs
                )

        // context = contextStruct(...pointersToCaptures)
        currentStatements?.add(
            Assignment(
            expression.location,
            contextName,
            Call(
                expression.location,
                contextType,
                contextConstructorCallee,
                pointersToCaptures
            )
        )
        )


        val functionName = namingCtx.makeUniqueName()
        val signature = HIRFunctionSignature(
            location = expression.location,
            name = functionName.toQualifiedName(),
            typeParams =
            if (expression.captures.types.isEmpty())
                null
            else
                expression.captures.types.map { HIRTypeParam(it.location, it.name) },
            params = expression.params.map { HIRParam(it.location, it.binder, it.type) } + listOf(
                HIRParam(expression.location, Binder(Identifier(expression.location, contextParamName)), Type.Ptr(contextType, isMutable = true))
            ),
            returnType = expression.returnType
        )

        val closureTypeStruct = getClosureTypeStruct(type)
        val closureStructConstructorRef = closureTypeStruct.constructorRef(expression.location)
        val closureConstructorCallee =
            if (type.to is Type.Void)
                closureStructConstructorRef
            else
                TypeApplication(
                    expression.location,
                    closureStructConstructorRef.type,
                    closureStructConstructorRef,
                    listOf(type.to)
                )

        val functionRef = GlobalRef(
            expression.location,
            signature.type,
            functionName.toQualifiedName()
        )
        val functionPointer = transformExpression(if (expression.captures.types.isEmpty()) {
            functionRef
        } else {
            TypeApplication(
                location = expression.location,
                type = signature.type,
                expression = functionRef,
                args = capturedTypeArgs
            )
        })
        // closure: closureType = closureConstructorRef(closureCtx, fnPtrRef)
        currentStatements?.add(
            Assignment(
            expression.location,
            closureName,
            Call(
                expression.location,
                closureType,
                closureConstructorCallee,
                args = listOf(
                    PointerCast(
                        expression.location,
                        Type.Void,
                        AddressOf(
                            expression.location,
                            Type.Ptr(Type.Void, isMutable = true),
                            contextName,
                        )
                    ),
                    PointerCast(
                        expression.location,
                        toPointerOfType = Type.Function(
                            from = listOf(),
                            to = expression.returnType,
                            traitRequirements = null,
                        ),
                        value = functionPointer
                    )
                )
            )
        )
        )

        captureStack.push(expression.captures.values.entries.mapIndexed { index, it ->
            it.key.name to CaptureInfo(contextDerefname, contextType, it.key.location, index)
        }.toMap())
        val statements = mutableListOf(
            Alloca(expression.location, name = contextDerefname, isMutable = true, type = contextType),
            Assignment(
                expression.location,
                contextDerefname,
                Load(
                    expression.location,
                    contextType,
                    ParamRef(
                        expression.location,
                        Type.Ptr(contextType, isMutable = false),
                        contextParamName,
                        Binder(Identifier(expression.location, contextDerefname))
                    )))
        )
        val oldBlockStatements = currentStatements
        currentStatements = statements
        expression.body.statements.forEach {
            currentStatements?.addAll(transformStatement(it))
        }
        currentStatements?.addAll(
                if (type.to is Type.Void)
                    listOf(ReturnVoid(expression.location))
                else
                    emptyList()
                )
        currentStatements = oldBlockStatements
        captureStack.pop()
        val body = HIRBlock(expression.body.location, namingCtx.makeName("entry"), statements)
        val fn = HIRDefinition.Function(
            expression.location,
            signature,
            mutableListOf(body)
        )

        definitions.add(fn)

        return ValRef(expression.location, closureType, closureName)
    }

    private fun makeAndAddClosureContextStruct(location: SourceLocation, captures: ClosureCaptures): HIRDefinition.Struct {
        val structName = namingCtx.makeUniqueName().toQualifiedName()
        val structDef = HIRDefinition.Struct(
            location = location,
            name = structName,
            typeParams =
                if (captures.types.isEmpty())
                    null
                else
                    captures.types.map { HIRTypeParam(it.location, it.name) },
            fields = captures.values.map { it.key.name to Type.Ptr(it.value, isMutable = true) }
        )
        definitions.add(structDef)

        return structDef

    }

    override fun transformParam(param: HIRParam): HIRParam {
        val original = super.transformParam(param)
        return original.copy(
            type =
                if (original.type is Type.Function)
                    getClosureType(original.type)
                else original.type
        )
    }

    private fun getClosureType(type: Type.Function): Type {
        val structDef = getClosureTypeStruct(type)
        val typeConstructor = Type.Constructor(structDef.name)
        return if (structDef.typeParams != null)
            Type.Application(
                typeConstructor,
                listOf(type.to)
            )
        else
            typeConstructor
    }

    private fun getClosureTypeStruct(type: Type.Function): HIRDefinition.Struct =
        if (type.to is Type.Void) structDefForClosureWithoutResult else structDefForClosureWithResult
}

private data class CaptureInfo(
    val contextName: Name,
    val contextType: Type,
    val declarationLocation: SourceLocation,
    val index: Int,
)