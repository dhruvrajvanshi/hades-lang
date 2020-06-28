package hadesc.hir.passes

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIRModule
import hadesc.hir.HIRTypeParam
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import java.util.concurrent.LinkedBlockingQueue
import kotlin.math.exp

class Monomorphization(
        private val ctx: Context
): HIRTransformer {
    private lateinit var oldModule: HIRModule
    private val specializationQueue = LinkedBlockingQueue<SpecializationRequest>()
    private var currentSpecialization: Map<SourceLocation, Type>? = null

    override fun transformModule(module: HIRModule): HIRModule {
        oldModule = module
        val newModule = super.transformModule(module)
        while (specializationQueue.isNotEmpty()) {
            addSpecialization(newModule, specializationQueue.take())
        }
        return newModule
    }

    private fun addSpecialization(module: HIRModule, request: SpecializationRequest) {
        val definitions = oldModule.findDefinitions(request.name)
        require(definitions.size == 1)
        val definition = definitions[0]
        require(definition is HIRDefinition.Function)
        val oldSpecialization = currentSpecialization
        require(definition.typeParams != null)
        require(definition.typeParams.size == request.typeArgs.size)

        currentSpecialization = definition.typeParams.zip(request.typeArgs).map {
            it.first.location to it.second
        }.toMap()

        module.addDefinition(
                HIRDefinition.Function(
                        location = definition.location,
                        returnType = lowerType(definition.returnType),
                        typeParams = null,
                        name = getSpecializedName(request.name, request.typeArgs),
                        params = definition.params.map { transformParam(it) },
                        body = transformBlock(definition.body),
                        receiverType = definition.receiverType?.let { lowerType(it) }
                )
        )

        currentSpecialization = oldSpecialization
    }

    override fun transformTypeParam(param: HIRTypeParam): HIRTypeParam {
        requireUnreachable()
    }

    override fun lowerParamRefType(type: Type.ParamRef): Type {
        val specialization = currentSpecialization
        requireNotNull(specialization)
        return requireNotNull(specialization[type.name.location])
    }

    override fun transformFunctionDef(definition: HIRDefinition.Function): Collection<HIRDefinition> {
        if (definition.typeParams == null) {
            return super.transformFunctionDef(definition)
        }
        return listOf()
    }

    override fun transformStructDef(definition: HIRDefinition.Struct): Collection<HIRDefinition> {
        if (definition.typeParams == null) {
            return super.transformStructDef(definition)
        }
        return listOf()
    }

    override fun transformCall(expression: HIRExpression.Call): HIRExpression {
        if (expression.typeArgs == null) {
            return super.transformCall(expression)
        }
        val specializedCallee = generateSpecialization(expression.callee, expression.typeArgs)
        return HIRExpression.Call(
                location = expression.location,
                typeArgs = null,
                type = lowerType(expression.type),
                args = expression.args.map { transformExpression(it) },
                callee = specializedCallee
        )
    }

    private fun generateSpecialization(expression: HIRExpression, typeArgs: List<Type>): HIRExpression = when(expression) {
        is HIRExpression.MethodRef -> {
            TODO()
        }
        is HIRExpression.GlobalRef -> {
            val name = getSpecializedName(expression.name, typeArgs.map { lowerType(it) })
            val globalFunction = oldModule.findGlobalFunctionDef(expression.name)
            require(globalFunction.typeParams != null)
            require(globalFunction.typeParams.size == typeArgs.size)
            val substitution = globalFunction.typeParams.zip(typeArgs).map {
                it.first.location to lowerType(it.second)
            }.toMap()
            val type = lowerType(expression.type.applySubstitution(substitution))
            HIRExpression.GlobalRef(
                    expression.location,
                    type,
                    name
            )
        }
        else -> requireUnreachable()
    }

    private val queuedSpecializationSet = mutableSetOf<QualifiedName>()
    private fun getSpecializedName(name: QualifiedName, typeArgs: List<Type>): QualifiedName {
        val specializedName = specializeName(name, typeArgs)
        if (specializedName !in queuedSpecializationSet) {
            queuedSpecializationSet.add(specializedName)
            enqueueSpecialization(name, typeArgs)
        }
        return specializedName
    }

    private fun enqueueSpecialization(name: QualifiedName, typeArgs: List<Type>) {
        specializationQueue.add(SpecializationRequest(name, typeArgs.map { lowerType(it) }))
    }

    private fun specializeName(name: QualifiedName, typeArgs: List<Type>): QualifiedName {
        return QualifiedName(listOf(
                *name.names.toTypedArray(),
                ctx.makeName("$"),
                ctx.makeName(typeArgs.map { lowerType(it) }.joinToString(",") { it.prettyPrint() })
        ))
    }

//    override fun transformInterfaceDef(definition: HIRDefinition.Interface): Collection<HIRDefinition> {
//    }

    override fun lowerTypeApplication(type: Type.Application): Type {
        TODO()
    }
}

data class SpecializationRequest(
        val name: QualifiedName,
        val typeArgs: List<Type>
)