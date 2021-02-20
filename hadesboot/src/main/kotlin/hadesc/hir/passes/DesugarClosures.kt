package hadesc.hir.passes

import hadesc.Name
import hadesc.analysis.ClosureCaptures
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.Context
import hadesc.hir.*
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.types.Type
import libhades.collections.Stack
import java.nio.file.Path

class DesugarClosures(val ctx: Context): HIRTransformer {
    private val definitions = mutableListOf<HIRDefinition>()

    private val closureCtxFieldName = ctx.makeName("ctx")
    private val closureFunctionPtrName = ctx.makeName("functionPtr")
    private val closureCtxFieldIndex = 0
    private val closureFuncPtrFieldIndex = 1
    private val captureStack = Stack<Map<Name, CaptureInfo>>()

    private val structDefForClosureWithResult by lazy {
        val location = SourceLocation(
            file = SourcePath(Path.of("builtin.ClosureWithResult")),
            start = Position(0, 0),
            stop = Position(0, 0)
        )
        val structName = ctx.makeName("\$builtin.ClosureWithResult")
        val typeParamName = ctx.makeName("T")
        val typeParam = Binder(Identifier(location, typeParamName))
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = listOf(HIRTypeParam(location, typeParamName)),
            fields = listOf(
                closureCtxFieldName to Type.Ptr(Type.Void, isMutable = true),
                closureFunctionPtrName to
                        Type.Ptr(Type.Function(
                            from = emptyList(), traitRequirements = emptyList(), to = Type.ParamRef(typeParam)), isMutable = false)
            ),
            name = structName.toQualifiedName()
        )
        definitions.add(def)
        def

    }
    private var currentBlockStatements = mutableListOf<HIRStatement>()
    private val structDefForClosureWithoutResult by lazy {
        val location = SourceLocation(
            file = SourcePath(Path.of("builtin.ClosureWithoutResult")),
            start = Position(0, 0),
            stop = Position(0, 0)
        )
        val structName = ctx.makeName("\$builtin.ClosureWithoutResult")
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = null,
            fields = listOf(
                closureCtxFieldName to Type.Ptr(Type.Void, isMutable = true),
                closureFunctionPtrName to
                        Type.Ptr(Type.Function(
                            from = listOf(
                                Type.Ptr(Type.Void, isMutable = true)
                            ), traitRequirements = emptyList(), to = Type.Void), isMutable = false)
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
        val result = HIRModule(definitions)
        logger().debug("Before closure desugaring: ${result.prettyPrint()}")
        return result
    }

    override fun transformBlock(body: HIRBlock): HIRBlock {
        val oldStatements = currentBlockStatements
        currentBlockStatements = mutableListOf()

        for (statement in body.statements) {
            currentBlockStatements.addAll(transformStatement(statement))
        }
        val statements = currentBlockStatements

        currentBlockStatements = oldStatements
        return HIRBlock(body.location, statements)
    }

    override fun transformInvokeClosure(expression: HIRExpression.InvokeClosure): HIRExpression {
        val closureRef = transformExpression(expression.closure)
        return HIRExpression.Call(
            expression.location,
            expression.type,
            HIRExpression.PointerCast(
                closureRef.location,
                toPointerOfType = Type.Function(
                    from = expression.args.map { it.type } + listOf( Type.Ptr(Type.Void, isMutable = true) ),
                    to = expression.type,
                    traitRequirements = null
                ),
                value = HIRExpression.GetStructField(
                    closureRef.location,
                    type = Type.Ptr(Type.Function(from = listOf(Type.Ptr(Type.Void, isMutable = true)), to = expression.type, traitRequirements = null), isMutable = false),
                    lhs = closureRef,
                    name = closureFunctionPtrName,
                    index = closureFuncPtrFieldIndex
                )
            ),
            expression.args.map { transformExpression(it) } + HIRExpression.GetStructField(
                closureRef.location,
                type = Type.Ptr(Type.Void, isMutable = false),
                lhs = closureRef,
                name = closureCtxFieldName,
                index = closureCtxFieldIndex,
            )
        )
    }


    override fun transformValRef(expression: HIRExpression.ValRef): HIRExpression {
        for (captures in captureStack) {
            val capture = captures[expression.name]
            if (capture != null) {
                val (contextName, contextType) = capture
                return HIRExpression.Load(
                    expression.location,
                    expression.type,
                    HIRExpression.GetStructField(
                        expression.location,
                        Type.Ptr(expression.type, isMutable = true),
                        lhs = HIRExpression.ParamRef(
                            expression.location,
                            contextType,
                            contextName
                        ),
                        name = expression.name,
                        index = capture.index
                    )
                )
            }
        }
        return super.transformValRef(expression)
    }

    override fun transformClosure(expression: HIRExpression.Closure): HIRExpression {
        val type = expression.type
        require(type is Type.Function)

        val contextStruct = makeAndAddClosureContextStruct(expression.location, expression.captures)
        val contextType = Type.Constructor(null, contextStruct.name)
        val contextName = ctx.makeUniqueName()
        val contextParamName = ctx.makeName("\$ctx")

        val closureName = ctx.makeUniqueName()
        val closureType = getClosureType(type)

        // val contextName: contextType
        currentBlockStatements.add(HIRStatement.ValDeclaration(
            expression.location,
            contextName,
            isMutable = true,
            contextType
        ))

        // val closureName: closureType
        currentBlockStatements.add(HIRStatement.ValDeclaration(
            expression.location,
            closureName,
            isMutable = false,
            closureType
        ))

        val pointersToCaptures = expression.captures.values.map {
            HIRExpression.AddressOf(
                expression.location,
                Type.Ptr(it.value, isMutable = true),
                it.key.name
            )
        }
        // context = contextStruct(...pointersToCaptures)
        currentBlockStatements.add(HIRStatement.Assignment(
            expression.location,
            contextName,
            HIRExpression.Call(
                expression.location,
                contextType,
                HIRExpression.GlobalRef(
                    expression.location,
                    contextStruct.constructorType,
                    contextStruct.name
                ),
                pointersToCaptures
            )
        ))


        val functionName = ctx.makeUniqueName()
        val signature = HIRFunctionSignature(
            location = expression.location,
            name = functionName.toQualifiedName(),
            typeParams =
            if (expression.captures.types.isEmpty())
                null
            else
                expression.captures.types.map { HIRTypeParam(it.location, it.name) },
            params = expression.params.map { HIRParam(it.location, it.name, it.type) } + listOf(
                HIRParam(expression.location, contextParamName, contextType)
            ),
            returnType = expression.returnType
        )

        val closureTypeStruct = getClosureTypeStruct(type)
        // closure: closureType = closureConstructorRef(closureCtx, fnPtrRef)
        currentBlockStatements.add(HIRStatement.Assignment(
            expression.location,
            closureName,
            HIRExpression.Call(
                expression.location,
                closureType,
                closureTypeStruct.constructorRef(expression.location),
                args = listOf(
                    HIRExpression.PointerCast(
                        expression.location,
                        Type.Void,
                        HIRExpression.AddressOf(
                            expression.location,
                            Type.Ptr(Type.Void, isMutable = true),
                            contextName,
                        )),
                    HIRExpression.PointerCast(
                        expression.location,
                        toPointerOfType = Type.Function(
                            from = listOf(Type.Ptr(Type.Void, isMutable = true)),
                            to = expression.returnType,
                            traitRequirements = null,
                        ),
                        value = HIRExpression.GlobalRef(
                            expression.location,
                            signature.type,
                            functionName.toQualifiedName()
                        ))
                )
            )
        )
        )

        captureStack.push(expression.captures.values.entries.mapIndexed { index, it ->
            it.key.name to CaptureInfo(contextParamName, contextType, index)
        }.toMap())
        val statements = expression.body.statements.flatMap { transformStatement(it) } + (
                if (type.to is Type.Void)
                    listOf(HIRStatement.ReturnVoid(expression.location))
                else
                    emptyList()
            )
        val body = HIRBlock(expression.body.location, statements)
        val fn = HIRDefinition.Function(
            expression.location,
            signature,
            body
        )

        definitions.add(fn)

        return HIRExpression.ValRef(expression.location, closureType, closureName)
    }

    private fun makeAndAddClosureContextStruct(location: SourceLocation, captures: ClosureCaptures): HIRDefinition.Struct {
        require(captures.types.isEmpty())
        val structName = ctx.makeUniqueName().toQualifiedName()
        val structDef = HIRDefinition.Struct(
            location = location,
            name = structName,
            typeParams = null,
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
        val typeConstructor = Type.Constructor(
            null,
            structDef.name,
        )
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
    val index: Int,
)