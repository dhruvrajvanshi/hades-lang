package hadesc.checker

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.TypeBinding
import hadesc.resolver.ValueBinding
import hadesc.types.Type
import java.util.*
import kotlin.math.min

@OptIn(ExperimentalStdlibApi::class)
class Checker(
    private val ctx: Context
) {

    private val binderTypes = MutableNodeMap<Binder, Type>()
    private val expressionTypes = MutableNodeMap<Expression, Type>()
    private val annotationTypes = MutableNodeMap<TypeAnnotation, Type>()
    private val returnTypeStack = Stack<Type>()
    private val typeArguments = MutableNodeMap<Expression, List<Type>>()

    private val genericInstantiations = mutableMapOf<Long, Type>()
    private var _nextGenericInstance = 0L

    fun typeOfExpression(expression: Expression): Type = expressionTypes.computeIfAbsent(expression) {
        val decl = ctx.resolver.getDeclarationContaining(expression)
        checkDeclaration(decl)
        requireNotNull(expressionTypes[expression])
    }

    private fun resolveQualifiedTypeVariable(node: HasLocation, path: QualifiedPath): Type? {
        val struct = ctx.resolver.resolveQualifiedStructDef(path)
        if (struct == null) {
            error(node.location, Diagnostic.Kind.UnboundType(path.identifiers.last().name))
            return null
        }
        val instanceType = typeOfStructInstance(struct)
        return if (struct.typeParams == null) {
            instanceType
        } else {
            require(instanceType is Type.Application)
            require(instanceType.callee is Type.Constructor)
            instanceType.callee
        }
    }

    private fun resolveTypeVariable(name: Identifier): Type? {
        return when (val binding = ctx.resolver.resolveTypeVariable(name)) {
            null -> return null
            is TypeBinding.Struct -> {
                val instanceType = typeOfStructInstance(binding.declaration)
                if (binding.declaration.typeParams == null) {
                    instanceType
                } else {
                    require(instanceType is Type.Application)
                    require(instanceType.callee is Type.Constructor)
                    instanceType.callee
                }
            }
            is TypeBinding.TypeParam -> Type.ParamRef(binding.binder)
        }
    }

    fun annotationToType(annotation: TypeAnnotation): Type = annotationTypes.computeIfAbsent(annotation) {
        val declaration = ctx.resolver.getDeclarationContaining(annotation)
        checkDeclaration(declaration)
        requireNotNull(annotationTypes[annotation])
    }

    fun typeOfBinder(binder: Binder): Type = binderTypes.computeIfAbsent(binder) {
        val decl = ctx.resolver.getDeclarationContaining(binder)
        checkDeclaration(decl)
        requireNotNull(binderTypes[binder])
    }

    private fun inferAnnotation(annotation: TypeAnnotation, allowIncomplete: Boolean = false): Type {
        val type = when (annotation) {
            is TypeAnnotation.Error -> Type.Error
            is TypeAnnotation.Var -> when (annotation.name.name.text) {
                "Void" -> Type.Void
                "Bool" -> Type.Bool
                "Byte" -> Type.Byte
                "CInt" -> Type.CInt
                else -> {
                    val typeBinding = resolveTypeVariable(annotation.name)
                    if (typeBinding != null) {
                        typeBinding
                    } else {
                        error(annotation, Diagnostic.Kind.UnboundType(annotation.name.name))
                        Type.Error
                    }
                }
            }
            is TypeAnnotation.Ptr -> Type.RawPtr(inferAnnotation(annotation.to))
            is TypeAnnotation.Application -> {
                val callee = inferAnnotation(annotation.callee, allowIncomplete = true)
                val args = annotation.args.map { inferAnnotation(it) }
                checkTypeApplication(annotation, callee, args)
                Type.Application(
                    callee,
                    args
                )
            }
            is TypeAnnotation.Qualified -> {
                val typeBinding = resolveQualifiedTypeVariable(annotation, annotation.qualifiedPath)
                if (typeBinding != null) {
                    typeBinding
                } else {
                    error(annotation, Diagnostic.Kind.UnboundType(annotation.qualifiedPath.identifiers.first().name))
                    Type.Error
                }
            }
        }
        annotationTypes[annotation] = type
        if (!allowIncomplete && type is Type.Constructor && type.params != null) {
            error(annotation, Diagnostic.Kind.IncompleteType(type.params.size))
        }
        return type
    }

    private fun checkTypeApplication(annotation: TypeAnnotation.Application, callee: Type, args: List<Type>) {
        // TODO: Check if args are compatible with callee type
    }

    fun isTypeEqual(t1: Type, t2: Type): Boolean {
        return t1 == t2
    }

    private fun typeOfStructConstructor(declaration: Declaration.Struct): Type {
        return typeOfBinder(declaration.binder)
    }

    fun typeOfStructInstance(declaration: Declaration.Struct): Type {
        val constructorType = typeOfStructConstructor(declaration)
        require(constructorType is Type.Function)
        return constructorType.to
    }

    fun typeOfStructMembers(declaration: Declaration.Struct): Map<Name, Type> {
        declareStruct(declaration)
        return requireNotNull(structFieldTypes[declaration])
    }

    fun getTypeArgs(call: Expression): List<Type>? {
        return typeArguments[call]
    }

    fun checkDeclaration(declaration: Declaration) = when (declaration) {
        is Declaration.Error -> {
        }
        is Declaration.ImportAs -> {
        }
        is Declaration.FunctionDef -> checkFunctionDef(declaration)
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.Struct -> checkStructDef(declaration)
        is Declaration.ConstDefinition -> checkConstDef(declaration)
    }

    private fun checkConstDef(declaration: Declaration.ConstDefinition) {
        declareGlobalConst(declaration)
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        val functionType = declareFunctionDef(declaration)
        withReturnType(functionType.to) {
            checkBlock(declaration.body)
        }
    }

    private fun withReturnType(returnType: Type, fn: () -> Unit) {
        returnTypeStack.push(returnType)
        fn()
        require(returnTypeStack.isNotEmpty())
        returnTypeStack.pop()
    }

    private fun declareFunctionDef(declaration: Declaration.FunctionDef): Type.Function {
        val cached = binderTypes[declaration.name]
        if (cached != null) {
            require(cached is Type.Function)
            return cached
        }
        val paramTypes = mutableListOf<Type>()
        for (param in declaration.params) {
            val type = if (param.annotation != null) {
                inferAnnotation(param.annotation)
            } else {
                Type.Error
            }
            bindValue(param.binder, type)
            paramTypes.add(type)
        }
        val receiverType = if (declaration.thisParam != null) {
            inferAnnotation(declaration.thisParam.annotation)
        } else {
            null
        }
        val returnType = inferAnnotation(declaration.returnType)
        val type = Type.Function(
            receiver = receiverType,
            from = paramTypes,
            to = returnType,
            typeParams = declaration.typeParams?.map { Type.Param(it.binder) }
        )
        bindValue(declaration.name, type)
        return type
    }

    private fun checkBlock(block: Block) {
        for (member in block.members) {
            checkBlockMember(member)
        }
    }

    private fun checkBlockMember(member: Block.Member): Unit = when (member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> {
            checkStatement(member.statement)
        }
    }

    private fun checkStatement(statement: Statement): Unit = when (statement) {
        is Statement.Return -> {
            if (returnTypeStack.isEmpty()) {
                requireUnreachable()
            } else {
                val returnType = returnTypeStack.peek()
                checkExpression(returnType, statement.value)
            }
        }
        is Statement.Val -> checkValStatement(statement)
        is Statement.While -> checkWhileStatement(statement)
        is Statement.If -> checkIfStatement(statement)
        is Statement.Error -> { }
    }

    private fun checkIfStatement(statement: Statement.If) {
        checkExpression(Type.Bool, statement.condition)
        checkBlock(statement.ifTrue)
        if (statement.ifFalse != null) {
            checkBlock(statement.ifFalse)
        }
    }

    private fun checkWhileStatement(statement: Statement.While) {
        checkExpression(Type.Bool, statement.condition)
        checkBlock(statement.body)
    }

    private val checkedValStatements = mutableSetOf<SourceLocation>()
    private fun checkValStatement(statement: Statement.Val) {
        if (checkedValStatements.contains(statement.location)) {
            return
        }
        val typeAnnotation = statement.typeAnnotation
        val type = if (typeAnnotation != null) {
            val expected = inferAnnotation(typeAnnotation)
            checkExpression(expected, statement.rhs)
            expected
        } else {
            inferExpression(statement.rhs)
        }

        bindValue(statement.binder, type)

        checkedValStatements.add(statement.location)
    }

    private fun inferExpression(expression: Expression): Type {
        val ty = when (expression) {
            is Expression.Error -> Type.Error
            is Expression.Var -> inferVar(expression)
            is Expression.Call -> inferCall(expression)
            is Expression.Property -> inferProperty(expression)
            is Expression.ByteString -> Type.RawPtr(Type.Byte)
            is Expression.BoolLiteral -> Type.Bool
            is Expression.This -> inferThis(expression)
            is Expression.NullPtr -> {
                error(expression, kind = Diagnostic.Kind.AmbiguousExpression)
                Type.RawPtr(Type.Error)
            }
            is Expression.IntLiteral -> Type.CInt
            is Expression.Not -> {
                checkExpression(Type.Bool, expression.expression)
                Type.Bool
            }
        }
        expressionTypes[expression] = ty
        return ty
    }

    private fun inferThis(expression: Expression.This): Type {
        val thisParam = ctx.resolver.resolveThisParam(expression)
        if (thisParam == null) {
            error(expression, Diagnostic.Kind.UnboundThis)
            return Type.Error
        }
        return inferAnnotation(thisParam.annotation)
    }

    private fun inferProperty(expression: Expression.Property): Type {
        val globalBinding = ctx.resolver.resolveModuleProperty(expression)
        return if (globalBinding != null) {
            inferBinding(globalBinding)
        } else {
            val lhsType = inferExpression(expression.lhs)
            val ownPropertyType = when (lhsType) {
                Type.Error -> Type.Error
                is Type.Struct -> {
                    lhsType.memberTypes[expression.property.name]
                }
                is Type.RawPtr -> null
                is Type.Function,
                is Type.ParamRef,
                is Type.GenericInstance,
                Type.Byte,
                Type.Void,
                Type.CInt,
                Type.Bool -> {
                    null
                }
                is Type.Application -> {
                    inferTypeApplicationProperty(expression, lhsType, expression.property)
                }
                is Type.Constructor -> TODO()
            }
            if (ownPropertyType != null) {
                ownPropertyType
            } else {
                val extensionPropertyType = inferExtensionProperty(expression, lhsType, expression.property)
                if (extensionPropertyType != null) {
                    extensionPropertyType
                } else {
                    error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
                    Type.Error
                }
            }
        }
    }

    private fun inferTypeApplicationProperty(lhs: Expression.Property, lhsType: Type.Application, property: Identifier): Type? =
        when (lhsType.callee) {
            is Type.Constructor -> {
                val binder = requireNotNull(lhsType.callee.binder)
                when (val binding = ctx.resolver.resolveTypeVariable(binder.identifier)) {
                    is TypeBinding.Struct -> {
                        if (binding.declaration.typeParams == null) {
                            null
                        } else {
                            val members = typeOfStructMembers(binding.declaration)
                            val fieldType = members[property.name]
                            if (fieldType == null) {
                                inferExtensionProperty(lhs, lhsType, property)
                            } else {
                                val substitution = binding.declaration.typeParams.mapIndexed { index, it ->
                                    it.binder.location to lhsType.args.elementAtOrElse(index) { Type.Error }
                                }.toMap()

                                fieldType.applySubstitution(substitution)
                            }
                        }
                    }
                    else -> {
                        null
                    }
                }

            }
            else -> {
                null
            }
        }

    /**
     * TODO: For generic extension methods, we only unify using the this type provided because
     *       we don't have access to arguments yet. We should unify the types of
     *       the passed method args as well. Not doing it right now because it requires
     *       some restructuring.
     */
    private fun inferExtensionProperty(lhs: Expression.Property, lhsType: Type, property: Identifier): Type? {
        return getExtensionDefAndType(lhs, lhsType, property)?.second
    }

    private val extensionDefs = MutableNodeMap<Expression.Property, Declaration.FunctionDef>()

    fun getExtensionDef(lhs: Expression.Property): Declaration.FunctionDef? {
        return extensionDefs[lhs]
    }

    private fun getExtensionDefAndType(lhs: Expression.Property, lhsType: Type, property: Identifier): Pair<Declaration.FunctionDef, Type>? {
        val extensionDefs = ctx.resolver.extensionDefsInScope(property, property).toList()
        for (def in extensionDefs) {
            require(def.thisParam != null)
            val thisParamType = inferAnnotation(def.thisParam.annotation)
            if (isTypeEqual(thisParamType, lhsType)) {
                this.extensionDefs[lhs] = def
                return def to typeOfBinder(def.name)
            }
            if (def.typeParams != null) {
                val substitution = mutableMapOf<SourceLocation, Type.GenericInstance>()
                def.typeParams.forEach {
                    substitution[it.binder.location] = makeGenericInstance(it.binder)
                }
                val functionType = typeOfBinder(def.name)

                val thisParamTypeInstance = thisParamType.applySubstitution(substitution)

                if (isAssignableTo(source = lhsType, destination = thisParamTypeInstance)) {
                    this.extensionDefs[lhs] = def
                    return def to functionType
                }
            }
        }
        return null
    }


    private fun makeGenericInstance(binder: Binder): Type.GenericInstance {
        _nextGenericInstance++
        return Type.GenericInstance(binder, _nextGenericInstance)
    }

    private fun inferCall(expression: Expression.Call): Type {
        val calleeType = inferExpression(expression.callee)
        if (calleeType is Type.Function) {
            val substitution = mutableMapOf<SourceLocation, Type.GenericInstance>()
            calleeType.typeParams?.forEach {
                substitution[it.binder.location] = makeGenericInstance(it.binder)
            }
            val len = min(calleeType.from.size, expression.args.size)
            val to = calleeType.to.applySubstitution(substitution)
            if (calleeType.receiver != null) {
                require(expression.callee is Expression.Property)
                val expected = calleeType.receiver.applySubstitution(substitution)
                val found = expression.callee.lhs
                checkExpression(expected, found)
            }
            for (index in 0 until len) {
                val expected = calleeType.from[index].applySubstitution(substitution)
                val found = expression.args[index].expression
                checkExpression(expected, found)
            }

            if (calleeType.from.size > expression.args.size) {
                error(expression, Diagnostic.Kind.MissingArgs(required = calleeType.from.size))
            } else if (calleeType.from.size < expression.args.size) {
                error(expression, Diagnostic.Kind.TooManyArgs(required = calleeType.from.size))
                for (index in len + 1 until expression.args.size) {
                    inferExpression(expression.args[index].expression)
                }
            }
            if (expression.callee is Expression.Property) {
                applyInstantiations(expression.callee)
            }
            for (arg in expression.args) {
                applyInstantiations(arg.expression)
            }
            val typeArgs = mutableListOf<Type>()
            calleeType.typeParams?.forEach {
                val generic = requireNotNull(substitution[it.binder.location])
                val instance = genericInstantiations[generic.id]
                typeArgs.add(
                    if (instance == null) {
                        error(expression.args.firstOrNull() ?: expression, Diagnostic.Kind.UninferrableTypeParam(it.binder))
                        Type.Error
                    } else {
                        instance
                    }
                )
            }
            if (calleeType.typeParams != null) {
                typeArguments[expression] = typeArgs
                typeArguments[expression.callee] = typeArgs
            }
            return applyInstantiations(to)
        } else {
            for (arg in expression.args) {
                inferExpression(arg.expression)
            }
            if (calleeType != Type.Error) {
                error(expression, Diagnostic.Kind.TypeNotCallable(calleeType))
            }
            return Type.Error
        }
    }


    private fun applyInstantiations(type: Type): Type = when (type) {
        Type.Error,
        Type.Byte,
        Type.Void,
        Type.CInt,
        is Type.ParamRef,
        Type.Bool -> type
        is Type.RawPtr -> Type.RawPtr(type.to)
        is Type.Function -> Type.Function(
            receiver = if (type.receiver != null) applyInstantiations(type.receiver) else null,
            typeParams = type.typeParams,
            from = type.from.map { applyInstantiations(it) },
            to = applyInstantiations(type.to)
        )
        is Type.Struct ->
            Type.Struct(
                constructor = type.constructor,
                memberTypes = type.memberTypes.mapValues { applyInstantiations(it.value) }
            )
        is Type.GenericInstance -> {
            genericInstantiations[type.id] ?: type
        }
        is Type.Application -> {
            Type.Application(
                applyInstantiations(type.callee),
                type.args.map { applyInstantiations(it) }
            )
        }
        is Type.Constructor -> type
    }

    private fun applyInstantiations(expression: Expression) {
        val ty = expressionTypes[expression] ?: inferExpression(expression)
        val instance = applyInstantiations(ty)
        expressionTypes[expression] = instance
    }

    private fun checkExpression(expected: Type, expression: Expression) = when {
        expression is Expression.NullPtr && expected is Type.RawPtr -> {
            expressionTypes[expression] = expected
        }
        else -> {
            val exprType = inferExpression(expression)
            checkAssignability(expression.location, destination = expected, source = exprType)
        }
    }

    private fun checkAssignability(location: SourceLocation, source: Type, destination: Type) {
        if (!isAssignableTo(source = source, destination = destination)) {
            error(location, Diagnostic.Kind.TypeNotAssignable(source = source, destination = destination))
        }
    }

    private fun isAssignableTo(source: Type, destination: Type): Boolean = when {
        source is Type.Error || destination is Type.Error -> {
            true
        }
        source is Type.CInt && destination is Type.CInt -> true
        source is Type.Bool && destination is Type.Bool -> {
            true
        }
        source is Type.Byte && destination is Type.Byte -> {
            true
        }
        source is Type.ParamRef && destination is Type.ParamRef
                && source.name.location == destination.name.location -> {
            true
        }
        source is Type.RawPtr && destination is Type.RawPtr ->
            isAssignableTo(source.to, destination.to)
        source is Type.Struct && destination is Type.Struct && source.constructor == destination.constructor -> {
            true
        }
        source is Type.Constructor && destination is Type.Constructor && source.name == destination.name -> {
            true
        }

        source is Type.Application && destination is Type.Application -> {
            val calleeAssignable = isAssignableTo(source.callee, destination.callee)
            if (!calleeAssignable) {
                false
            } else {
                if (source.args.size != destination.args.size) {
                    false
                } else {
                    source.args.zip(destination.args).all { (sourceParam, destinationParam) ->
                        val assignable = isAssignableTo(destination = destinationParam, source = sourceParam)
                        assignable
                    }
                }
            }
        }
        destination is Type.GenericInstance -> {
            val destinationInstance = genericInstantiations[destination.id]
            if (destinationInstance != null) {
                isAssignableTo(source = source, destination = destinationInstance)
            } else {
                genericInstantiations[destination.id] = source
                true
            }
        }
        else -> {
            false
        }
    }

    private fun inferVar(expression: Expression.Var): Type {
        val binding = ctx.resolver.resolve(expression.name)
        if (binding == null) {
            error(expression, Diagnostic.Kind.UnboundVariable)
        }
        return when (binding) {
            null -> Type.Error
            else -> inferBinding(binding)
        }
    }

    private fun inferBinding(binding: ValueBinding) = when (binding) {
        is ValueBinding.GlobalFunction -> {
            declareFunctionDef(binding.declaration)
            requireNotNull(binderTypes[binding.declaration.name])
        }
        is ValueBinding.ExternFunction -> {
            declareExternFunctionDef(binding.declaration)
            requireNotNull(binderTypes[binding.declaration.binder])
        }
        is ValueBinding.FunctionParam -> {
            declareFunctionDef(binding.declaration)
            requireNotNull(binderTypes[binding.param.binder])
        }
        is ValueBinding.ValBinding -> {
            checkValStatement(binding.statement)
            requireNotNull(binderTypes[binding.statement.binder])
        }
        is ValueBinding.Struct -> {
            declareStruct(binding.declaration)
            requireNotNull(binderTypes[binding.declaration.binder])
        }
        is ValueBinding.GlobalConst -> {
            declareGlobalConst(binding.declaration)
            requireNotNull(binderTypes[binding.declaration.name])
        }
    }

    private fun error(node: HasLocation, kind: Diagnostic.Kind) {
        error(node.location, kind)
    }

    private fun error(location: SourceLocation, kind: Diagnostic.Kind) {
        ctx.diagnosticReporter.report(location, kind)
    }

    private fun checkExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        declareExternFunctionDef(declaration)
    }

    private fun declareExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        if (binderTypes[declaration.binder] != null) {
            return
        }
        val paramTypes = declaration.paramTypes.map { inferAnnotation(it) }
        val returnType = inferAnnotation(declaration.returnType)
        val type = Type.Function(
            receiver = null,
            from = paramTypes,
            to = returnType,
            typeParams = null // extern functions can't be generic
        )
        bindValue(declaration.binder, type)
    }

    private fun bindValue(binder: Binder, type: Type) {
        binderTypes[binder] = type
    }

    private fun checkStructDef(declaration: Declaration.Struct) {
        declareStruct(declaration)
    }

    private fun declareGlobalConst(declaration: Declaration.ConstDefinition) {
        if (binderTypes[declaration.name] != null) {
            return
        }
        val rhsType = inferExpression(declaration.initializer)
        when (rhsType) {
            is Type.CInt -> {}
            is Type.Bool -> {}
            else -> error(declaration.initializer, Diagnostic.Kind.NotAConst)
        }
        bindValue(declaration.name, rhsType)
    }

    private val structFieldTypes = MutableNodeMap<Declaration.Struct, Map<Name, Type>>()
    private fun declareStruct(declaration: Declaration.Struct) {
        if (binderTypes[declaration.binder] != null) {
            return
        }
        val fieldTypes = mutableMapOf<Name, Type>()
        for (member in declaration.members) {
            exhaustive(when (member) {
                Declaration.Struct.Member.Error -> {
                }
                is Declaration.Struct.Member.Field -> {
                    val ty = inferAnnotation(member.typeAnnotation)
                    require(fieldTypes[member.binder.identifier.name] == null)
                    fieldTypes[member.binder.identifier.name] = ty
                    Unit
                }
            })
        }
        val name = ctx.resolver.qualifiedStructName(declaration)
        val typeParams = declaration.typeParams?.map { Type.Param(it.binder) }
        val constructor = Type.Constructor(declaration.binder, name, typeParams)
        structFieldTypes[declaration] = fieldTypes
        val instanceType = if (typeParams != null) {
            Type.Application(constructor, typeParams.map { Type.ParamRef(it.binder) })
        } else {
            Type.Struct(constructor, fieldTypes)
        }
        val constructorParamTypes = fieldTypes.values.toList()
        val constructorType = Type.Function(
            from = constructorParamTypes,
            to = instanceType,
            typeParams = declaration.typeParams?.map { Type.Param(it.binder) },
            receiver = null
        )
        bindValue(declaration.binder, constructorType)
    }
}

private class MutableNodeMap<T : HasLocation, V> {
    private val map = mutableMapOf<SourceLocation, V>()

    fun computeIfAbsent(key: T, compute: () -> V): V {
        val existing = map[key.location]
        if (existing != null) {
            return existing
        }
        val value = compute()
        map[key.location] = value
        return value
    }

    operator fun get(key: T): V? {
        return map[key.location]
    }

    operator fun set(key: T, value: V) {
        map[key.location] = value
    }
}

