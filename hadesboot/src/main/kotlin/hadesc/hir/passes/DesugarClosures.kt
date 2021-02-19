package hadesc.hir.passes

import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.Context
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIRModule
import hadesc.hir.HIRTypeParam
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
            name = QualifiedName(listOf(structName))
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
        require(type.to !is Type.Void)

        val closureType = getClosureType(type)

        TODO()
    }

    private fun getClosureType(type: Type.Function): Type {
        require(type.to !is Type.Void) { TODO() }
        val structDef = structDefForClosureWithResult
        return Type.Application(
            Type.Constructor(
            null,
            structDef.name,
            ),
            listOf(type.to)
        )
    }
}