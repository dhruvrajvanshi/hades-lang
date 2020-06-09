package hadesc.ir.passes

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.ir.*
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.profile
import hadesc.types.Type
import java.util.concurrent.LinkedBlockingQueue

class SpecializeGenerics(
    private val ctx: Context,
    private val oldModule: IRModule
) {
    private var currentFunction: IRFunctionDef? = null
    private val module = IRModule()
    private val builder = IRBuilder()

    private val specializationQueue = LinkedBlockingQueue<SpecializationRequest>()
    private var currentSpecialization: Map<SourceLocation, Type>? = null

    fun run(): IRModule = profile("SpecializeGenerics::run") {
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
                    is IRConstDef -> visitConstDef(definition)
                    is IRInterfaceDef -> TODO()
                    is IRImplementationDef -> TODO()
                }
            )
        }
        while (specializationQueue.isNotEmpty()) {
            visitSpecializationRequest(specializationQueue.take())
            require(currentSpecialization == null)
        }
        return module
    }

    private fun visitConstDef(definition: IRConstDef) {
        module.addConstDef(
            lowerGlobalName(definition.name),
            lowerType(definition.type),
            lowerValue(definition.initializer)
        )
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
        val block = IRBlock(definition.entryBlock.name)

        val fn = module.addGlobalFunctionDef(
            definition.signature.location,
            lowerGlobalName(definition.name),
            lowerType(definition.type) as Type.Function,
            typeParams = null,
            receiverType = null,
            params = definition.params.map { lowerParam(it) },
            entryBlock = block,
            constraints = listOf()
        )
        currentFunction = fn
        lowerBlock(oldBlock = definition.entryBlock, newBlock = block)
        for (block in definition.blocks) {
            val newBlock = IRBlock(block.name)
            fn.appendBlock(newBlock)
            lowerBlock(newBlock = newBlock, oldBlock = block)
        }
    }


    private fun lowerBlock(oldBlock: IRBlock, newBlock: IRBlock): IRBlock {
        builder.position = newBlock
        for (statement in oldBlock) {
            exhaustive(
                when (statement) {
                    is IRReturnInstruction -> visitReturnStatement(statement)
                    is IRReturnVoidInstruction -> {
                        builder.buildRetVoid()
                        Unit
                    }
                    is IRCall -> visitCallStatement(statement)
                    is IRAlloca -> visitAlloca(statement)
                    is IRStore -> visitStore(statement)
                    is IRLoad -> visitLoad(statement)
                    is IRNot -> visitNot(statement)
                    is IRBr -> visitBranch(statement)
                    is IRJump -> visitJump(statement)
                    is IRBinOp -> visitBinOp(statement)
                    is IRSwitch -> requireUnreachable()
                    is IRDefer -> requireUnreachable()
                }
            )
        }
        return newBlock
    }

    private fun visitJump(statement: IRJump) {
        builder.buildJump(
            statement.location,
            statement.label
        )
    }

    private fun visitBinOp(binOp: IRBinOp) {
        builder.buildBinOp(
                lowerType(binOp.type),
                lowerLocalName(binOp.name),
                lowerValue(binOp.lhs),
                binOp.operator,
                lowerValue(binOp.rhs)
        )
    }

    private fun visitNot(statement: IRNot) {
        builder.buildNot(lowerType(statement.type), statement.location, statement.name, lowerValue(statement.arg))
    }

    private fun visitBranch(statement: IRBr) {
        builder.buildBranch(
            statement.location,
            lowerValue(statement.condition),
            statement.ifTrue,
            statement.ifFalse
        )

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
        is IRCIntConstant -> value
        is IRNullPtr -> IRNullPtr(type = lowerType(value.type), location = value.location)
        is IRMethodRef -> requireUnreachable()
        is IRSizeOf -> lowerSizeOf(value, typeArgs)
        is IRPointerCast -> IRPointerCast(
                type = lowerType(value.type),
                toPointerOfType = lowerType(value.toPointerOfType),
                arg = lowerValue(value.arg),
                location = value.location
        )
        is IRAggregate -> IRAggregate(
                type = lowerType(value.type),
                location = value.location,
                values = value.values.map { lowerValue(it) }
        )
        is IRGetElementPointer -> IRGetElementPointer(
                type = lowerType(value.type),
                location = value.location,
                offset = value.offset,
                ptr = lowerValue(value.ptr)
        )
    }

    private fun lowerSizeOf(value: IRSizeOf, typeArgs: List<Type>?): IRValue {
        require(typeArgs == null)
        return IRSizeOf(
                type = lowerType(value.type),
                ofType = lowerType(value.ofType),
                location = value.location
        )
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
        return when (val binding = requireNotNull(oldModule.resolveGlobal(name))) {
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
            is IRBinding.ConstDef -> {
                require(typeArgs == null)
                variable
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
        node: HasLocation,
        typeArgs: List<Type>?,
        thisArg: IRValue? = null
    ): IRValue {
        if (binding.def.typeParams == null) {
            require(typeArgs == null)
            require(thisArg == null)
            return builder.buildVariable(binding.type, node.location, lowerGlobalName(binding.def.name))
        }
        requireNotNull(typeArgs)
        val (loweredName, loweredType) = enqueueFunctionSpecialization(binding.def, typeArgs.map { lowerType(it) })
        return builder.buildVariable(
            loweredType,
            node.location,
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
        val instanceType = Type.Constructor(binder = null, name = name.name, params = null)
        return name to Type.Function(
            typeParams = null,
            from = fieldTypes.map { it.value }.toList(),
            to = instanceType,
            receiver = null
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
        val fn = module.addGlobalFunctionDef(
            def.signature.location,
            name, fnType,
            typeParams = null,
            params = params,
            receiverType = null,
            entryBlock = body,
            constraints = emptyList()
        )
        currentSpecialization = makeSubstitution(requireNotNull(def.typeParams), typeArgs)
        currentFunction = fn
        lowerBlock(oldBlock = def.entryBlock, newBlock = body)
        for (block in def.blocks) {
            val newBlock = IRBlock(block.name)
            fn.appendBlock(newBlock)
            lowerBlock(oldBlock = block, newBlock = newBlock)
        }
        currentSpecialization = null
    }

    private fun getSpecializedFunctionType(
        def: IRFunctionDef,
        typeArgs: List<Type>
    ): Pair<IRGlobalName, Type.Function> {
        requireNotNull(def.signature.typeParams)
        val substitution = makeSubstitution(def.signature.typeParams, typeArgs)
        val loweredTypeArgs = typeArgs.map { lowerType(it.applySubstitution(substitution)) }
        val name = specializationName(def.name, loweredTypeArgs)
        require(def.signature.typeParams.size == typeArgs.size)
        val paramTypes = def.params.map { lowerType(it.type.applySubstitution(substitution)) }
        val returnType = lowerType(def.type.to.applySubstitution(substitution))
        return name to Type.Function(
            typeParams = null,
            from = paramTypes,
            to = returnType,
            receiver = null
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

    private fun visitReturnStatement(statement: IRReturnInstruction) {
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
            Type.Error -> requireUnreachable()
            Type.Byte,
            Type.Void,
            Type.CInt,
            Type.Bool -> {
                type
            }
            is Type.Ptr -> {
                Type.Ptr(lowerType(type.to), isMutable = type.isMutable)
            }
            is Type.Function -> {
                Type.Function(
                    typeParams = null,
                    from = type.from.map { lowerType(it) },
                    to = lowerType(type.to),
                    receiver = null
                )
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
            Type.Size -> type
            is Type.ThisRef -> requireUnreachable()
            is Type.UntaggedUnion -> Type.UntaggedUnion(type.members.map { lowerType(it) })
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
