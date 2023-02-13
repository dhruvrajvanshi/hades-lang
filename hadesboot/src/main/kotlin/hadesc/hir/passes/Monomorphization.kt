package hadesc.hir.passes

import hadesc.Name
import hadesc.analysis.TraitClause
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitResolver
import hadesc.analysis.TypeAnalyzer
import hadesc.assertions.requireUnreachable
import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Substitution
import hadesc.types.Type
import hadesc.types.toSubstitution
import java.util.concurrent.LinkedBlockingQueue

class Monomorphization(
    override val namingCtx: NamingContext
) : AbstractHIRTransformer() {
    private val log = logger(Monomorphization::class.java)
    private lateinit var oldModule: HIRModule
    private val specializationQueue = LinkedBlockingQueue<SpecializationRequest>()
    private var currentSpecialization: Substitution? = null
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

    override fun lowerSelectType(type: Type.Select): Type {
        val impl = getTraitImpl(type.traitName, type.traitArgs)
        return requireNotNull(impl.associatedTypes[type.associatedTypeName])
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

    override fun lowerParamRefType(type: Type.ParamRef): Type {
        val specialization = currentSpecialization
        requireNotNull(specialization)
        return requireNotNull(specialization[type.name.location])
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
            check(exprType is Type.TypeFunction)
            val substitution = makeSubstitution(exprType.params.map { HIRTypeParam(it.binder.location, it.binder.name) }, typeArgs)
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
            it.first.location to lowerType(it.second)
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

    override fun lowerRawPtrType(type: Type.Ptr): Type {
        // specialized names should be uniform for both pointer types
        //
        // mutability is checked at the frontend, after that, we don't really
        // care about it
        return Type.Ptr(lowerType(type.to), isMutable = false)
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

    override fun transformImplementationDef(definition: HIRDefinition.Implementation): Collection<HIRDefinition> {
        return emptyList()
    }

    private val globalTraitClauses by lazy {
        allImpls.map { impl ->
            TraitClause.Implementation(
                def = impl,
                params = impl.typeParams?.map { Type.Param(it.toBinder()) } ?: emptyList(),
                traitRef = impl.traitName,
                arguments = impl.traitArgs,
                requirements = impl.traitRequirements.map { TraitRequirement(it.traitRef, it.arguments, it.negated) }

            )
        }
    }
    override fun transformTraitMethodRef(expression: HIRExpression.TraitMethodRef): HIROperand {
        val impl = getTraitImpl(expression.traitName, expression.traitArgs)
        val implMethodNames = impl.methods
        return HIRExpression.GlobalRef(
            expression.location,
            lowerType(expression.type),
            checkNotNull(implMethodNames[expression.methodName])
        )
    }

    private val monoImplCache = mutableMapOf<QualifiedName, MonoImpl>()
    private fun getTraitImpl(traitName: QualifiedName, requiredTraitArgs: List<Type>): MonoImpl {
        val implName = specializeName(traitName, requiredTraitArgs)
        val cached = monoImplCache[implName]
        if (cached != null) {
            return cached
        }
        // required traitName[...traitArgs]

        // T == traitName
        // subst = make_subst(...Ps)

        // ------------------------------------------
        // isCandidate(impl [Ps...] T[...Ps])

        //
        // ------------
        // isCandidate(Printable[Box[X, Y]], impl [A, B] Printable[Box[A, B]] where Printable[A], Printable[B]

        val traitArgs = requiredTraitArgs
            .map { lowerType(it) }
            .map { if (it is Type.Constructor) specializedTypes[it.name] ?: it else it }

        val eligibleCandidates = mutableListOf<Pair<HIRDefinition.Implementation, Substitution>>()
        @Suppress("LoopWithTooManyJumpStatements")
        for (candidate in allImpls) {
            val typeAnalyzer = TypeAnalyzer()
            if (candidate.traitName != traitName) continue
            val substitutionMap =
                candidate.typeParams?.associate { it.location to typeAnalyzer.makeGenericInstance(it.toBinder()) }
                    ?: emptyMap()
            val substitution = substitutionMap.toSubstitution()
            if (!traitArgs.zip(candidate.traitArgs).all { (requiredType, actualType) ->
                typeAnalyzer.isTypeAssignableTo(
                        destination = requiredType,
                        source = actualType.applySubstitution(substitution)
                    )
            }
            ) {
                continue
            }

            val traitResolver = TraitResolver(
                TraitResolver.Env(globalTraitClauses),
                typeAnalyzer
            )

            if (!candidate.traitRequirements.all { requirement ->
                traitResolver.isSatisfied(
                        requirement.copy(
                                arguments = requirement.arguments.map { it.applySubstitution(substitution) }
                            )
                    )
            }
            ) {
                continue
            }
            val subst = (
                substitutionMap.mapValues {
                    requireNotNull(typeAnalyzer.getInstantiatedType(it.value))
                }
                )
            eligibleCandidates.add(
                candidate to subst.toSubstitution()
            )
        }
        require(eligibleCandidates.isNotEmpty()) {
            "Trait implementation not found: ${traitName.mangle()}[${traitArgs.joinToString(", ") { it.prettyPrint() }}]"
        }
        require(eligibleCandidates.size == 1) {
            "Overlapping trait implementations."
        }
        val (candidate, subst) = eligibleCandidates.first()
        val result = MonoImpl(
            generateImplMethodMap(candidate, subst),

            generateImplAssociatedTypeMap(candidate, subst)
        )
        monoImplCache[implName] = result
        return result
    }

    private fun generateImplAssociatedTypeMap(candidate: HIRDefinition.Implementation, subst: Substitution): Map<Name, Type> {
        return candidate.typeAliases.mapValues { it.value.applySubstitution(subst) }
    }

    private fun generateImplMethodMap(impl: HIRDefinition.Implementation, substitution: Substitution): Map<Name, QualifiedName> {
        val typeArgs = impl.typeParams?.map { requireNotNull(substitution[it.location]) } ?: emptyList()

        val implName = specializeName(QualifiedName(listOf(impl.name)), typeArgs)

        val result = mutableMapOf<Name, QualifiedName>()
        impl.functions.forEach { fn ->
            require(fn.name.size == 1)
            val name = implName.append(fn.name.first)
            result[fn.name.first] = name
        }

        impl.functions.forEach { fn ->
            require(fn.name.size == 1)
            val name = implName.append(fn.name.first)
            queuedSpecializationSet.add(name)
            if (typeArgs.isEmpty()) {
                transformFunctionDef(fn, name).forEach { newDefinitions.add(it) }
            } else {
                specializationQueue.add(
                    SpecializationRequest.FunctionDef(
                        name = name,
                        typeArgs = typeArgs,
                        def = fn.copy(
                            signature = fn.signature.copy(
                                typeParams = impl.typeParams
                            )
                        )
                    )
                )
            }
        }
        return result
    }

    private val HIRDefinition.Implementation.name get() =
        namingCtx.makeName("${traitName.mangle()}[${traitArgs.joinToString(",") { it.prettyPrint() } }]")
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

data class MonoImpl(
    val methods: Map<Name, QualifiedName>,
    val associatedTypes: Map<Name, Type>
)
