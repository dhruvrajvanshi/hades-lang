package hadesc.hir.passes

import hadesc.Name
import hadesc.analysis.TraitClause
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitResolver
import hadesc.analysis.TypeAnalyzer
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Substitution
import hadesc.types.Type
import java.util.concurrent.LinkedBlockingQueue

class Monomorphization(
        private val ctx: Context
): HIRTransformer {
    private lateinit var oldModule: HIRModule
    private val specializationQueue = LinkedBlockingQueue<SpecializationRequest>()
    private var currentSpecialization: Map<SourceLocation, Type>? = null
    private val allImpls by lazy {
        oldModule.definitions.filterIsInstance<HIRDefinition.Implementation>()
    }
    private val newDefinitions = mutableListOf<HIRDefinition>()

    override fun transformModule(oldModule: HIRModule): HIRModule {
        this.oldModule = oldModule
        val newModule = super.transformModule(oldModule)
        while (specializationQueue.isNotEmpty()) {
            addSpecialization(newModule, specializationQueue.take())
        }
        newModule.definitions.addAll(newDefinitions)
        logger().debug("HIR after monomorphization:\n${newModule.prettyPrint()}")
        return newModule
    }

    private fun addSpecialization(module: HIRModule, request: SpecializationRequest) {
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
                                        request,
                                        definition.signature),
                                body = transformBlock(definition.body)
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
    private fun specializeFunctionSignature(request: SpecializationRequest, definition: HIRFunctionSignature): HIRFunctionSignature {
        return HIRFunctionSignature(
            location = definition.location,
            returnType = lowerType(definition.returnType),
            typeParams = null,
            name = getSpecializedName(request.name, request.typeArgs),
            params = definition.params.map { transformParam(it) },
        )
    }

    override fun transformTypeParam(param: HIRTypeParam): HIRTypeParam {
        requireUnreachable()
    }

    override fun lowerParamRefType(type: Type.ParamRef): Type {
        val specialization = currentSpecialization
        requireNotNull(specialization)
        return requireNotNull(specialization[type.name.location])
    }

    override fun transformFunctionDef(definition: HIRDefinition.Function, newName: QualifiedName?): Collection<HIRDefinition> {
        if (definition.typeParams == null) {
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

    override fun transformTypeApplication(expression: HIRExpression.TypeApplication): HIRExpression {
        return generateSpecialization(expression.expression, expression.args)
    }

    private fun generateSpecialization(expression: HIRExpression, typeArgs: List<Type>): HIRExpression = when(expression) {
        is HIRExpression.GlobalRef -> {
            val name = getSpecializedName(expression.name, typeArgs.map { lowerType(it) })
            when (val definition = oldModule.findGlobalDefinition(expression.name)) {
                is HIRDefinition.Function -> {
                    val substitution = makeSubstitution(definition.typeParams, typeArgs)
                    val type = lowerType(expression.type.applySubstitution(substitution))
                    HIRExpression.GlobalRef(
                            expression.location,
                            type,
                            name
                    )
                }
                is HIRDefinition.Struct -> {
                    val substitution = makeSubstitution(definition.typeParams, typeArgs)
                    val type = lowerType(expression.type.applySubstitution(substitution))
                    HIRExpression.GlobalRef(
                            expression.location,
                            type,
                            name
                    )
                }
                else -> {
                    requireUnreachable()
                }
            }
        }
        else -> requireUnreachable()
    }

    private fun makeSubstitution(typeParams: List<HIRTypeParam>?, typeArgs: List<Type>): Map<SourceLocation, Type> {
        require(typeParams != null)
        require(typeParams.size == typeArgs.size)
        return typeParams.zip(typeArgs).map {
            it.first.location to lowerType(it.second)
        }.toMap()
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
                ctx.makeName("\$[" +
                        typeArgs.map { lowerType(it) }.joinToString(",") { it.prettyPrint() } +
                "]")
        ))
    }

    override fun lowerTypeApplication(type: Type.Application): Type {
        require(type.callee is Type.Constructor)
        val typeName = type.callee.name
        val definition = oldModule.findGlobalDefinition(typeName)
        require(definition is HIRDefinition.Struct)
        val specializedName = getSpecializedName(typeName, type.args.map { lowerType(it) })
        return Type.Constructor(binder = null, name = specializedName)
    }

    override fun transformImplementationDef(definition: HIRDefinition.Implementation): Collection<HIRDefinition> {
        return emptyList()
    }

    private val globalTraitClauses by lazy {
        allImpls.map { impl ->
            TraitClause.Implementation(
                params = impl.typeParams?.map { Type.Param(it.toBinder()) } ?: emptyList(),
                traitRef = impl.traitName,
                arguments = impl.traitArgs,
                requirements = impl.traitRequirements.map { TraitRequirement(it.traitRef, it.arguments) }

            )
        }
    }
    override fun transformTraitMethodCall(expression: HIRExpression.TraitMethodCall): HIRExpression {
        // required traitName[...traitArgs]

        // T == traitName
        // subst = make_subst(...Ps)

        // ------------------------------------------
        // isCandidate(impl [Ps...] T[...Ps])


        //
        // ------------
        // isCandidate(Printable[Box[X, Y]], impl [A, B] Printable[Box[A, B]] where Printable[A], Printable[B]

        val traitName = expression.traitName
        val traitArgs = expression.traitArgs.map { lowerType(it) }
        val typeAnalyzer = TypeAnalyzer()

        val eligibleCandidates = mutableListOf<Pair<HIRDefinition.Implementation, Substitution>>()
        for (candidate in allImpls) {
            val typeAnalyzer = TypeAnalyzer()
            if (candidate.traitName != traitName) continue
            val substitution = candidate.typeParams
                ?.map { it.location to typeAnalyzer.makeGenericInstance(it.toBinder()) }
                ?.toMap()
                ?: emptyMap()
            if (!traitArgs.zip(candidate.traitArgs).all { (requiredType, actualType) ->
                    typeAnalyzer.isTypeAssignableTo(
                        destination = requiredType,
                        source = actualType.applySubstitution(substitution))
            }) continue

            val traitResolver = TraitResolver(
                TraitResolver.Env(globalTraitClauses),
                typeAnalyzer
            )

            if (!candidate.traitRequirements.all { requirement ->
                    traitResolver.isTraitImplemented(
                        requirement.traitRef,
                        requirement.arguments.map { it.applySubstitution(substitution) }) })
                            continue
            eligibleCandidates.add(candidate to
                    (substitution.mapValues {
                        requireNotNull(typeAnalyzer.getInstantiatedType(it.value)) }))
        }
        require(eligibleCandidates.size == 1) {
            "Overlapping trait implementations."
        }
        val (impl, substitution) = eligibleCandidates.first()
        val implMethodNames = generateImpl(impl)
        return HIRExpression.Call(
                location = expression.location,
                type = expression.type,
                callee = HIRExpression.GlobalRef(
                        expression.location,
                        Type.Ptr(Type.Void, isMutable = false),
                        requireNotNull(implMethodNames[expression.methodName])),
                args = expression.args.map { transformExpression(it) }
        )
    }

    private val generateImplCache = mutableMapOf<Name, Map<Name, QualifiedName>>()
    private fun generateImpl(impl: HIRDefinition.Implementation): Map<Name, QualifiedName> {
        require(impl.typeParams.isNullOrEmpty())
        val implName = impl.name

        val cached = generateImplCache[impl.name]

        if (cached != null) {
            return cached
        }

        val result = mutableMapOf<Name, QualifiedName>()
        impl.functions.forEach { fn ->
            require(fn.name.size == 1)
            val name = QualifiedName(listOf(implName, fn.name.first))
            result[fn.name.first] = name
        }
        generateImplCache[impl.name] = result

        impl.functions.forEach { fn ->
            require(fn.name.size == 1)
            val name = QualifiedName(listOf(implName, fn.name.first))
            newDefinitions.addAll(transformFunctionDef(fn, newName = name))
        }
        return result
    }

    private val HIRDefinition.Implementation.name get() =
        ctx.makeName("impl\$${traitName.mangle()}[${traitArgs.joinToString(",") { it.prettyPrint() } }]")


}

data class SpecializationRequest(
        val name: QualifiedName,
        val typeArgs: List<Type>
)