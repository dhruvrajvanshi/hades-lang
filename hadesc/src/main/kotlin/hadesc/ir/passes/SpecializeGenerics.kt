package hadesc.ir.passes

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.ir.*
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.types.Type
import java.util.concurrent.LinkedBlockingQueue

class SpecializeGenerics(val ctx: Context, val oldModule: IRModule) {
    private val module = IRModule()
    val builder = IRBuilder()

    private val log = logger()

    private val specializationQueue = LinkedBlockingQueue<SpecializationRequest>()
    private var currentSpecialization: Map<SourceLocation, Type>? = null

    fun run(): IRModule {
        try {
            for (definition in oldModule) {
                exhaustive(
                    when (definition) {
                        is IRFunctionDef -> visitFunctionDef(definition)
                        is IRStructDef -> {
                            visitStructDef(definition)
                        }
                        is IRExternFunctionDef -> {
                            visitExternFunctionDef(definition)
                        }
                    }
                )
            }
            while (specializationQueue.isNotEmpty()) {
                visitSpecializationRequest(specializationQueue.take())
                require(currentSpecialization == null)
            }
            log.debug("after specialization:\n${module.prettyPrint()}")
        } catch (e: Error) {
            log.debug("module:\n${module.prettyPrint()}")
            throw e
        }
        return module
    }

    private fun visitSpecializationRequest(request: SpecializationRequest): Unit = when (request) {
        is SpecializationRequest.Struct -> specializeStruct(request.definition, request.typeArgs)
        is SpecializationRequest.Function -> specializeFunction(request.definition, request.typeArgs)
    }

    private fun visitExternFunctionDef(definition: IRExternFunctionDef) {
        module.add(definition)
    }

    private fun visitStructDef(definition: IRStructDef) {
        if (definition.typeParams != null) {
            return
        }
        module.add(definition)
    }

    private fun visitFunctionDef(definition: IRFunctionDef) {
        if (definition.typeParams != null) {
            return
        }
        val block = IRBlock()

        module.addGlobalFunctionDef(
            lowerGlobalName(definition.name),
            lowerType(definition.type) as Type.Function,
            typeParams = null,
            params = definition.params.map { lowerParam(it) },
            body = block
        )

        builder.withinBlock(block) {
            visitBlock(definition.body)
        }
    }

    private fun visitBlock(block: IRBlock) {
        for (statement in block) {
            exhaustive(
                when (statement) {
                    is IRReturnStatement -> visitReturnStatement(statement)
                    is IRReturnVoidStatement -> {
                        builder.buildRetVoid()
                        Unit
                    }
                    is IRCall -> visitCallStatement(statement)
                    is IRAlloca -> visitAlloca(statement)
                    is IRStore -> visitStore(statement)
                    is IRLoad -> visitLoad(statement)
                }
            )
        }
    }

    private fun visitCallStatement(statement: IRCall) {
        builder.buildCall(
            name = lowerLocalName(statement.name),
            args = statement.args.map { lowerValue(it) },
            callee = lowerValue(statement.callee, statement.typeArgs),
            type = lowerType(statement.type),
            typeArgs = null,
            location = statement.location
        )
    }

    private fun lowerValue(value: IRValue, typeArgs: List<Type>? = null): IRValue = when (value) {
        is IRBool,
        is IRByteString -> value
        is IRVariable -> lowerVariable(value, typeArgs)
        is IRGetStructField -> lowerGetStructField(value, typeArgs)
    }

    private fun lowerGetStructField(value: IRGetStructField, typeArgs: List<Type>? = null): IRValue {
        val lhs = lowerValue(value.lhs)
        return builder.buildGetStructField(
            ty = lowerType(value.type, typeArgs),
            field = value.rhs,
            location = value.location,
            index = value.index,
            lhs = lhs
        )
    }

    private fun lowerVariable(value: IRVariable, typeArgs: List<Type>?): IRValue = when (value.name) {
        is IRLocalName -> lowerLocalVariable(value, value.name, typeArgs)
        is IRGlobalName -> lowerGlobalVariable(value, value.name, typeArgs)
    }

    private fun lowerGlobalVariable(variable: IRVariable, name: IRGlobalName, typeArgs: List<Type>?): IRValue {
        val binding = requireNotNull(oldModule.resolveGlobal(name))
        return when (binding) {
            is IRBinding.FunctionDef -> {
                lowerFunctionDefBinding(binding, variable, typeArgs)
            }
            is IRBinding.ExternFunctionDef -> {
                require(typeArgs == null)
                variable
            }
            is IRBinding.StructDef -> {
                lowerStructDefBinding(binding, variable, typeArgs)
            }
        }
    }

    private fun lowerStructDefBinding(
        binding: IRBinding.StructDef,
        variable: IRVariable,
        typeArgs: List<Type>?
    ): IRValue {
        if (binding.def.typeParams == null) {
            require(typeArgs == null)
            return variable
        }
        requireNotNull(typeArgs)
        val (loweredName, loweredType) = enqueueStructSpecialization(binding.def, typeArgs)
        return builder.buildVariable(
            loweredType,
            variable.location,
            loweredName
        )
    }

    private fun lowerFunctionDefBinding(
        binding: IRBinding.FunctionDef,
        variable: IRVariable,
        typeArgs: List<Type>?
    ): IRValue {
        if (binding.def.typeParams == null) {
            require(typeArgs == null)
            return variable
        }
        requireNotNull(typeArgs)
        val (loweredName, loweredType) = enqueueFunctionSpecialization(binding.def, typeArgs.map { lowerType(it) })
        return builder.buildVariable(
            loweredType,
            variable.location,
            loweredName
        )
    }

    private val queuedSpecializaionSet = mutableSetOf<IRGlobalName>()
    private fun enqueueStructSpecialization(def: IRStructDef, typeArgs: List<Type>): Pair<IRGlobalName, Type.Function> {
        val (name, constructorType) = getSpecializedStructConstructorType(def, typeArgs)
        // ensure that we don't generate same specializations multiple times
        if (queuedSpecializaionSet.contains(name)) {
            return name to constructorType
        }
        specializationQueue.put(SpecializationRequest.Struct(def, typeArgs.map { lowerType(it) }))
        queuedSpecializaionSet.add(name)
        return name to constructorType
    }

    private fun specializeStruct(def: IRStructDef, typeArgs: List<Type>) {
        val (name, constructorType) = getSpecializedStructConstructorType(def, typeArgs)
        requireNotNull(def.typeParams)
        require(def.typeParams.size == typeArgs.size)
        val substitution = makeSubstitution(def.typeParams, typeArgs)
        // this is only called from top level of the module so it is guaranteed that the types already lowered
        val fieldTypes = def.fields.mapValues { it.value.applySubstitution(substitution) }
        module.addStructDef(
            constructorType = constructorType,
            name = name,
            typeParams = null,
            fields = fieldTypes,
            instanceType = constructorType.to
        )
    }

    private fun getSpecializedStructConstructorType(
        def: IRStructDef,
        typeArgs: List<Type>
    ): Pair<IRGlobalName, Type.Function> {
        requireNotNull(def.typeParams)
        val loweredArgs = typeArgs.map { lowerType(it) }
        val name = specializationName(def.globalName, loweredArgs)
        require(def.typeParams.size == typeArgs.size)
        val substitution = makeSubstitution(def.typeParams, typeArgs)
        val fieldTypes = def.fields.mapValues { lowerType(it.value.applySubstitution(substitution)) }
        val instanceType = Type.Struct(
            Type.Constructor(binder = null, name = name.name, params = null),
            fieldTypes
        )
        return name to Type.Function(
            typeParams = null,
            from = fieldTypes.map { it.value }.toList(),
            to = instanceType
        )
    }


    private fun enqueueFunctionSpecialization(
        def: IRFunctionDef,
        typeArgs: List<Type>
    ): Pair<IRGlobalName, Type.Function> {
        val (name, functionType) = getSpecializedFunctionType(def, typeArgs)
        if (queuedSpecializaionSet.contains(name)) {
            return name to functionType
        }
        queuedSpecializaionSet.add(name)
        specializationQueue.put(SpecializationRequest.Function(def, typeArgs))
        return name to functionType
    }


    private fun specializeFunction(def: IRFunctionDef, typeArgs: List<Type>) {
        val (name, fnType) = getSpecializedFunctionType(def, typeArgs)
        val body = IRBlock()
        require(fnType.from.size == def.params.size)
        val params = fnType.from.zip(def.params).map { (type, param) ->
            IRParam(param.name, type = type, index = param.index, location = param.location, functionName = name)
        }
        module.addGlobalFunctionDef(
            name, fnType,
            typeParams = null,
            params = params,
            body = body
        )
        currentSpecialization = makeSubstitution(requireNotNull(def.typeParams), typeArgs)
        builder.withinBlock(body) {
            visitBlock(def.body)
        }
        currentSpecialization = null
    }

    private fun getSpecializedFunctionType(
        def: IRFunctionDef,
        typeArgs: List<Type>
    ): Pair<IRGlobalName, Type.Function> {
        requireNotNull(def.typeParams)
        val substitution = makeSubstitution(def.typeParams, typeArgs)
        val loweredTypeArgs = typeArgs.map { lowerType(it.applySubstitution(substitution)) }
        val name = specializationName(def.name, loweredTypeArgs)
        require(def.typeParams.size == typeArgs.size)
        val paramTypes = def.params.map { lowerType(it.type.applySubstitution(substitution)) }
        val returnType = lowerType(def.type.to.applySubstitution(substitution))
        return name to Type.Function(
            typeParams = null,
            from = paramTypes,
            to = returnType
        )
    }

    private fun makeSubstitution(params: List<IRTypeParam>, typeArgs: List<Type>): Map<SourceLocation, Type> {
        return params.zip(typeArgs).map { it.first.binderLocation to it.second }.toMap()
    }

    private fun specializationName(globalName: IRGlobalName, loweredArgs: List<Type>): IRGlobalName {
        return IRGlobalName(
            globalName.name.append(
                ctx.makeName("$[" + loweredArgs.joinToString(",") { it.prettyPrint() } + "]")
            )
        )
    }

    private fun lowerLocalVariable(variable: IRVariable, name: IRLocalName, typeArgs: List<Type>?): IRValue {
        return builder.buildVariable(
            ty = lowerType(variable.type, typeArgs),
            location = variable.location,
            name = lowerLocalName(name)
        )
    }

    private fun lowerLocalName(name: IRLocalName): IRLocalName {
        return name
    }

    private fun lowerGlobalName(name: IRGlobalName): IRGlobalName {
        return name
    }

    private fun visitReturnStatement(statement: IRReturnStatement) {
        builder.buildReturn(lowerValue(statement.value))
    }

    private fun visitAlloca(statement: IRAlloca) {
        builder.buildAlloca(
            name = lowerLocalName(statement.name),
            type = lowerType(statement.type)
        )
    }

    private fun visitStore(statement: IRStore) {
        builder.buildStore(
            ptr = lowerValue(statement.ptr),
            value = lowerValue(statement.value)
        )
    }

    private fun visitLoad(statement: IRLoad) {
        builder.buildLoad(
            name = lowerLocalName(statement.name),
            type = lowerType(statement.type),
            ptr = lowerValue(statement.ptr)
        )
    }

    private fun lowerParam(param: IRParam): IRParam {
        return param.copy(type = lowerType(param.type))
    }

    private fun lowerType(type: Type, typeArgs: List<Type>? = null): Type {
        assert(typeArgs == null || type is Type.Constructor)
        return when (type) {
            Type.Error,
            Type.Byte,
            Type.Void,
            Type.Bool -> {
                type
            }
            is Type.RawPtr -> {
                Type.RawPtr(lowerType(type.to))
            }
            is Type.Function -> {
                Type.Function(
                    typeParams = null,
                    from = type.from.map { lowerType(it) },
                    to = lowerType(type.to)
                )
            }
            is Type.Struct -> {
                // we don't need to recursively lower struct types
                // because this struct types are specialized by getSpecializedStructConstructor type
                // in the Type.Constructor branch
                // The only struct types this function is called with are already
                // fully lowered.
                type
            }
            is Type.Constructor -> {
                if (type.params == null) {
                    require(typeArgs == null)
                    type
                } else {
                    requireNotNull(typeArgs)
                    require(typeArgs.size == type.params.size)
                    val binding = oldModule.resolveGlobal(type.name)
                    require(binding is IRBinding.StructDef) {
                        "Binding $binding does not define a generic type"
                    }
                    val constructorType = enqueueStructSpecialization(binding.def, typeArgs).second
                    constructorType.to
                }
            }
            is Type.ParamRef -> {
                val specialization = requireNotNull(currentSpecialization)
                requireNotNull(specialization[type.name.location])
            }
            is Type.GenericInstance -> requireUnreachable()
            is Type.Application -> {
                lowerType(type.callee, type.args)
            }
        }
    }
}


sealed class SpecializationRequest {
    abstract val typeArgs: List<Type>

    data class Struct(
        val definition: IRStructDef,
        override val typeArgs: List<Type>
    ) : SpecializationRequest()

    data class Function(
        val definition: IRFunctionDef,
        override val typeArgs: List<Type>
    ) : SpecializationRequest()
}
