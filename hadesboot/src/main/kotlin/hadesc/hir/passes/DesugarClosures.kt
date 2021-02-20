package hadesc.hir.passes

import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.Context
import hadesc.hir.*
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import java.nio.file.Path

class DesugarClosures(val ctx: Context): HIRTransformer {
    private val definitions = mutableListOf<HIRDefinition>()

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
                ctx.makeName("ctx") to Type.Ptr(Type.Void, isMutable = true),
                ctx.makeName("functionPtr") to
                        Type.Ptr(Type.Function(
                            from = emptyList(), traitRequirements = emptyList(), to = Type.ParamRef(typeParam)), isMutable = false)
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
        val structName = ctx.makeName("\$builtin.ClosureWithoutResult")
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = null,
            fields = listOf(
                ctx.makeName("ctx") to Type.Ptr(Type.Void, isMutable = true),
                ctx.makeName("functionPtr") to
                        Type.Ptr(Type.Function(
                            from = emptyList(), traitRequirements = emptyList(), to = Type.Void), isMutable = false)
            ),
            name = structName.toQualifiedName()
        )
        definitions.add(def)
        def

    }

    override fun transformModule(oldModule: HIRModule): HIRModule {
        for (definition in oldModule.definitions) {
            definitions.addAll(transformDefinition(definition))
        }
        return HIRModule(definitions)
    }

    override fun transformClosure(expression: HIRExpression.Closure): HIRExpression {
        val type = expression.type
        require(type is Type.Function)

        val closureType = getClosureType(type)

        val statements = expression.body.statements.flatMap { transformStatement(it) } + (
                if (type.to is Type.Void)
                    listOf(HIRStatement.ReturnVoid(expression.location))
                else
                    emptyList()
            )
        val body = HIRBlock(expression.body.location, statements)
        val functionName = ctx.makeName("closureFn").toQualifiedName()
        val signature = HIRFunctionSignature(
            location = expression.location,
            name = functionName,
            typeParams =
                if (expression.captures.types.isEmpty())
                    null
                else
                    expression.captures.types.map { HIRTypeParam(it.location, it.name) },
            params = (expression.params.map { HIRParam(it.location, it.name, it.type) }),
            returnType = expression.returnType
        )
        val fn = HIRDefinition.Function(
            expression.location,
            signature,
            body
        )

        definitions.add(fn)

        return HIRExpression.GlobalRef(expression.location, type = Type.Ptr(fn.type, isMutable = false), fn.name)
    }

    override fun transformParam(param: HIRParam): HIRParam {
        val original = super.transformParam(param)
        return original.copy(
            type = if (original.type is Type.Function) Type.Ptr(
                original.type.copy(
                    from = original.type.from
                ), isMutable = false) else original.type
        )
    }

    private fun getClosureType(type: Type.Function): Type {
        val structDef = if (type.to is Type.Void) structDefForClosureWithoutResult else structDefForClosureWithResult
        return Type.Application(
            Type.Constructor(
            null,
            structDef.name,
            ),
            listOf(type.to)
        )
    }
}