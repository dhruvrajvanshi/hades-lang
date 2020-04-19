package hadesc.ir.passes

import hadesc.Name
import hadesc.context.Context
import hadesc.ir.*
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.types.Type

class SpecializeGenerics(val ctx: Context, val _module: IRModule) {
    private val newModule = IRModule()
    val builder = IRBuilder()

    private val log = logger()

    fun run(): IRModule {
        for (definition in _module.definitions.toList()) {
            val exhaustive = when (definition) {
                is IRFunctionDef -> visitFunctionDef(definition)
                is IRStructDef -> {
                    visitStructDef(definition)
                }
                is IRExternFunctionDef -> {
                    visitExternFunctionDef(definition)
                }
            }
        }
        log.debug("after specialization: ${newModule.prettyPrint()}")
        return newModule
    }

    private fun visitExternFunctionDef(definition: IRExternFunctionDef) {
        newModule.definitions.add(definition)
    }

    private fun visitStructDef(definition: IRStructDef) {
        require(definition.typeParams == null)
        newModule.definitions.add(definition)

    }

    private fun visitFunctionDef(definition: IRFunctionDef): Unit {
        if (definition.typeParams != null) {
            return
        }
        val block = IRBlock()
        builder.withinBlock(block) {
            for (statement in definition.body) {
                visitStatement(statement)
            }
        }

        newModule.addGlobalFunctionDef(
            definition.binder,
            null,
            definition.params,
            block
        )
    }

    private fun visitStatement(statement: IRStatement) = when (statement) {
        is IRCall -> visitCallStatement(statement)
        is IRValStatement -> {
            visitValStatement(statement)
        }
        is IRReturnStatement -> {
            visitReturnStatement(statement)
        }
        is IRReturnVoidStatement -> {
            builder.buildRetVoid()
            Unit
        }
        is IREmptyStatement -> {
        }
    }

    private fun visitValStatement(statement: IRValStatement): Unit {
        val value = visitValue(statement.initializer)
        builder.buildValStatement(
            statement.binder.copy(type = statement.binder.type.applySpecializations()),
            value
        )
    }

    private fun visitReturnStatement(statement: IRReturnStatement): Unit {
        val value = visitValue(statement.value)
        builder.buildReturn(value)
    }

    private fun visitValue(value: IRValue, typeArgs: List<Type>? = null): IRValue = when (value) {
        is IRBool -> value
        is IRByteString -> value
        is IRVariable -> {
            visitVariable(value, typeArgs)
        }
        is IRGetStructField -> {
            builder.buildGetStructField(
                value.type.applySpecializations(),
                value.location,
                visitValue(value.lhs),
                value.rhs,
                value.index
            )
        }
    }

    private fun visitVariable(value: IRVariable, typeArgs: List<Type>?): IRValue = when (value.binding) {
        is IRBinding.FunctionDef -> {
            if (typeArgs != null) {
                val newBinding = declareSpecializedFunctionDef(value.binding.def, typeArgs)
                builder.buildVariable(value.type.applySpecializations(), value.location, newBinding)
            } else {
                value
            }
        }
        is IRBinding.ExternFunctionDef -> TODO()
        is IRBinding.Local -> {
            builder.buildVariable(value.type.applySpecializations(), value.location, value.binding)
        }
        is IRBinding.StructDef -> TODO()
        is IRBinding.ParamRef -> {
            require(typeArgs == null)
            if (value.binding.def.typeParams == null) {
                value
            } else {
                requireNotNull(paramSubstitutions[value.binding.def.params[value.binding.index].location])
            }
        }
    }

    private fun visitCallStatement(call: IRCall) {
        val typeArgs = call.typeArgs
        if (typeArgs == null) {
            builder.addStatement(call)
            return
        }
        val newCallee = visitValue(call.callee, typeArgs.map { it.applySpecializations() })
        builder.buildCall(
            call.type.applySpecializations(),
            call.location,
            newCallee,
            typeArgs = null,
            args = call.args.map { visitValue(it) },
            name = call.name
        )
    }

    private val specializations = mutableMapOf<Name, IRBinding>()
    private fun declareSpecializedFunctionDef(def: IRFunctionDef, typeArgs: List<Type>): IRBinding {
        require(def.typeParams != null)
        require(def.typeParams.size == typeArgs.size)
        val specializationName = specializationName(def.binder.name, typeArgs)
        val existing = specializations[specializationName]
        if (existing != null) {
            return existing
        }
        val substitution = def.typeParams.zip(typeArgs).map { it.first.binderLocation to it.second }
        val s = substitution.toMap()
        val specializedParams =
            def.params.map { IRParam(IRBinder(it.binder.name, it.binder.type.applySubstitution(s)), it.location) }
        val fnType = def.type
        require(fnType is Type.Function)
        val specializedType = Type.Function(
            typeParams = null,
            from = fnType.from.map { it.applySubstitution(s) },
            to = fnType.to.applySubstitution(s)
        )
        val fn = newModule.addGlobalFunctionDef(
            IRBinder(specializationName, specializedType),
            typeParams = null,
            params = specializedParams,
            body = IRBlock()
        )

        val paramSubstitution = def.params.mapIndexed { index, param ->
            val specializedParam =
                IRParam(param.binder.copy(type = param.binder.type.applySpecializations()), param.location)
            param.location to builder.buildVariable(
                specializedParam.binder.type,
                specializedParam.location,
                IRBinding.ParamRef(fn, index)
            )
        }
        pushSpecialization(substitution, paramSubstitution)

        builder.withinBlock(fn.body) {
            for (statement in def.body) {
                visitStatement(statement)
            }
        }
        popSpecialization(substitution, paramSubstitution)
        return IRBinding.FunctionDef(fn)
    }

    private val substitution = mutableMapOf<SourceLocation, Type>()
    private val paramSubstitutions = mutableMapOf<SourceLocation, IRValue>()
    private fun pushSpecialization(
        specialization: List<Pair<SourceLocation, Type>>,
        paramSpecialization: List<Pair<SourceLocation, IRValue>>
    ) {

        for ((location, type) in specialization) {
            require(substitution[location] == null) {
                "Recursive generic substitution"
            }
            substitution[location] = type
        }


        for ((location, value) in paramSpecialization) {
            require(paramSubstitutions[location] == null) {
                "Recursive generic substitution"
            }
            paramSubstitutions[location] = value
        }


    }

    private fun popSpecialization(
        specialization: List<Pair<SourceLocation, Type>>,
        paramSpecialization: List<Pair<SourceLocation, IRValue>>
    ) {
        for ((location, _) in specialization) {
            require(substitution[location] != null) {
                "Nested specializations for $location, ${substitution}"
            }
            substitution.remove(location)
        }
        for ((location, _) in paramSpecialization) {
            require(paramSubstitutions[location] != null)
            paramSubstitutions.remove(location)
        }
    }

    private fun Type.applySpecializations(): Type {
        return applySubstitution(substitution)
    }


    private fun specializationName(name: Name, typeArgs: List<Type>): Name {
        return ctx.makeName(name.text + "$[${typeArgs.joinToString(",") { it.prettyPrint() }}]")
    }

}
