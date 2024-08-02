package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.NamingCtx
import hadesc.hir.*
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Substitution
import hadesc.types.Type
import hadesc.types.toSubstitution
import java.util.concurrent.LinkedBlockingQueue

class Monomorphization(
    override val namingCtx: NamingCtx
) : AbstractHIRTransformer() {
    private val log = logger(Monomorphization::class.java)
    private lateinit var oldModule: HIRModule
    private val specializationQueue = LinkedBlockingQueue<SpecializationRequest>()
    private var currentSpecialization: Substitution? = null
    private val newDefinitions = mutableListOf<HIRDefinition>()

    override fun transformModule(oldModule: HIRModule): HIRModule {
        this.oldModule = oldModule
        val newModule = super.transformModule(oldModule)
        while (specializationQueue.isNotEmpty()) {
            addSpecialization(newModule, specializationQueue.take())
        }
        newModule.definitions.addAll(newDefinitions)
        log.debug("HIR after monomorphization:\n${newModule.prettyPrint()}")
        return newModule
    }

    private fun addSpecialization(module: HIRModule, request: SpecializationRequest) {
        when (request) {
            is SpecializationRequest.ByName -> addByNameSpecialization(module, request)
            is SpecializationRequest.FunctionDef -> addFunctionDefSpecialization(module, request)
        }
    }

    private val generatedSpecializaionSet = mutableSetOf<QualifiedName>()
    private fun addFunctionDefSpecialization(module: HIRModule, request: SpecializationRequest.FunctionDef) {
        if (generatedSpecializaionSet.contains(request.name)) {
            return
        }
        generatedSpecializaionSet.add(request.name)
        val definition = request.def
        val oldSpecialization = currentSpecialization
        currentSpecialization = makeSubstitution(definition.typeParams, request.typeArgs)

        val signature = HIRFunctionSignature(
            location = definition.location,
            returnType = lowerType(definition.returnType),
            typeParams = null,
            name = request.name,
            params = definition.params.map { transformParam(it) }
        )
        module.addDefinition(
            HIRDefinition.Function(
                location = definition.location,
                signature = signature,
                basicBlocks = definition.basicBlocks.map { transformBlock(it) }.toMutableList()
            )
        )

        currentSpecialization = oldSpecialization
    }

    private fun addByNameSpecialization(module: HIRModule, request: SpecializationRequest.ByName) {
        val definitions = oldModule.findDefinitions(request.name)
        require(definitions.size == 1)
        val definition = definitions[0]

        val oldSpecialization = currentSpecialization
        when (definition) {
            is HIRDefinition.Function -> {
                currentSpecialization = makeSubstitution(definition.typeParams, request.typeArgs)

                module.addDefinition(
                    HIRDefinition.Function(
                        location = definition.location,
                        signature = specializeFunctionSignature(
                            request.name,
                            request.typeArgs,
                            definition.signature
                        ),
                        basicBlocks = definition.basicBlocks.map { transformBlock(it) }.toMutableList()
                    )
                )

                currentSpecialization = oldSpecialization
            }
            is HIRDefinition.Struct -> {
                currentSpecialization = makeSubstitution(definition.typeParams, request.typeArgs)

                module.addDefinition(
                    HIRDefinition.Struct(
                        location = definition.location,
                        name = getSpecializedName(request.name, request.typeArgs),
                        typeParams = null,
                        fields = definition.fields.map { it.first to lowerType(it.second) }
                    )
                )

                currentSpecialization = oldSpecialization
            }
            else -> {
                requireUnreachable()
            }
        }
    }

    override fun lowerGenericInstance(type: Type.GenericInstance): Type {
        requireUnreachable()
    }

    private fun specializeFunctionSignature(
        name: QualifiedName,
        typeArgs: List<Type>,
        definition: HIRFunctionSignature
    ): HIRFunctionSignature {
        return HIRFunctionSignature(
            location = definition.location,
            returnType = lowerType(definition.returnType),
            typeParams = null,
            name = getSpecializedName(name, typeArgs),
            params = definition.params.map { transformParam(it) }
        )
    }

    override fun transformTypeParam(param: HIRTypeParam): HIRTypeParam {
        requireUnreachable()
    }

    override fun lowerParamRefType(type: Type.Param): Type {
        val specialization = currentSpecialization
        requireNotNull(specialization)
        return requireNotNull(specialization[type.name.id])
    }

    override fun transformFunctionDef(definition: HIRDefinition.Function, newName: QualifiedName?): Collection<HIRDefinition> {
        if (definition.typeParams == null) {
            specializedFnRef.clear()
            return super.transformFunctionDef(definition, newName)
        }
        return listOf()
    }

    override fun transformStructDef(definition: HIRDefinition.Struct): Collection<HIRDefinition> {
        if (definition.typeParams == null) {
            return super.transformStructDef(definition)
        }
        return listOf()
    }

    private val specializedFnRef = mutableMapOf<Name, HIROperand>()
    override fun transformTypeApplication(statement: HIRStatement.TypeApplication): List<HIRStatement> {
        val specializedRef = generateSpecialization(statement.expression, statement.args)
        specializedFnRef[statement.name] = specializedRef
        return emptyList()
    }

    override fun transformLocalRef(expression: HIRExpression.LocalRef): HIROperand {
        val specialized = specializedFnRef[expression.name]
        if (specialized != null) {
            return specialized
        }
        return super.transformLocalRef(expression)
    }

    private fun generateSpecialization(expression: HIRExpression, typeArgs: List<Type>): HIROperand = when (expression) {
        is HIRExpression.GlobalRef -> {
            val name = getSpecializedName(expression.name, typeArgs.map { lowerType(it) })
            val definition = oldModule.findGlobalDefinition(expression.name)
            check(definition is HIRDefinition.Struct || definition is HIRDefinition.Function)
            val exprType = expression.type
            check(exprType is Type.ForAll)
            val substitution = makeSubstitution(exprType.params.map { HIRTypeParam(it.binder.location, it.binder.name, it.binder.id) }, typeArgs)
            val type = lowerType(exprType.body.applySubstitution(substitution))
            HIRExpression.GlobalRef(
                expression.location,
                type,
                name
            )
        }
        else -> requireUnreachable()
    }

    private fun makeSubstitution(typeParams: List<HIRTypeParam>?, typeArgs: List<Type>): Substitution {
        require(typeParams != null)
        require(typeParams.size == typeArgs.size)
        return typeParams.zip(typeArgs).associate {
            it.first.id to lowerType(it.second)
        }.toSubstitution()
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
        specializationQueue.add(SpecializationRequest.ByName(name, typeArgs.map { lowerType(it) }))
    }

    private fun specializeName(name: QualifiedName, typeArgs: List<Type>): QualifiedName {
        return QualifiedName(
            name.names + listOf(
                namingCtx.makeName(
                    "[" +
                        typeArgs.map { lowerType(it) }.joinToString(",") { it.prettyPrint() } +
                        "]"
                )
            )
        )
    }

    private val specializedTypes = mutableMapOf<QualifiedName, Type.Application>()
    override fun lowerTypeApplication(type: Type.Application): Type {
        require(type.callee is Type.Constructor)
        val typeName = type.callee.name
        val definition = oldModule.findGlobalDefinition(typeName)
        require(definition is HIRDefinition.Struct)
        val specializedName = getSpecializedName(typeName, type.args.map { lowerType(it) })
        specializedTypes[specializedName] = type.copy(args = type.args.map { lowerType(it) })
        return Type.Constructor(specializedName)
    }
}

sealed class SpecializationRequest {
    data class ByName(
        val name: QualifiedName,
        val typeArgs: List<Type>
    ) : SpecializationRequest()
    data class FunctionDef(
        val name: QualifiedName,
        val typeArgs: List<Type>,
        val def: HIRDefinition.Function
    ) : SpecializationRequest()
}
