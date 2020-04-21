package hadesc.checker

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.TypeBinding
import hadesc.resolver.ValueBinding
import hadesc.types.Type
import java.util.*
import kotlin.math.min

@OptIn(ExperimentalStdlibApi::class)
class Checker(val ctx: Context) {

    private val binderTypes = MutableNodeMap<Binder, Type>()
    private val expressionTypes = MutableNodeMap<Expression, Type>()
    private val annotationTypes = MutableNodeMap<TypeAnnotation, Type>()
    private val returnTypeStack = Stack<Type>()
    private val typeArguments = MutableNodeMap<Expression.Call, List<Type>>()

    private val genericInstantiations = mutableMapOf<Long, Type>()
    private var _nextGenericInstance = 0L

    fun typeOfExpression(expression: Expression): Type = expressionTypes.computeIfAbsent(expression) {
        val decl = ctx.resolver.getDeclarationContaining(expression)
        checkDeclaration(decl)
        requireNotNull(expressionTypes[expression])
    }


    private fun resolveTypeVariable(name: Identifier): Type? {
        val binding = ctx.resolver.resolveTypeVariable(name)
        return when (binding) {
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

    fun typeOfStructConstructor(declaration: Declaration.Struct): Type {
        return typeOfBinder(declaration.binder)
    }

    fun typeOfStructInstance(declaration: Declaration.Struct): Type {
        val constructorType = typeOfStructConstructor(declaration)
        require(constructorType is Type.Function)
        val instanceType = constructorType.to
        return instanceType
    }

    fun typeOfStructMembers(declaration: Declaration.Struct): Map<Name, Type> {
        declareStruct(declaration)
        return requireNotNull(structFieldTypes[declaration])
    }

    fun getTypeArgs(call: Expression.Call): List<Type>? {
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
        val returnType = inferAnnotation(declaration.returnType)
        val type = Type.Function(
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
        is Statement.Error -> {
        }
    }

    val checkedValStatements = mutableSetOf<SourceLocation>()
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
        }
        expressionTypes[expression] = ty
        return ty
    }

    private fun inferProperty(expression: Expression.Property): Type {
        val globalBinding = ctx.resolver.resolveModuleProperty(expression)
        return if (globalBinding != null) {
            inferBinding(globalBinding)
        } else {
            when (val lhsType = inferExpression(expression.lhs)) {
                Type.Error -> Type.Error
                is Type.Struct -> {
                    val memberType = lhsType.memberTypes[expression.property.name]
                    if (memberType != null) {
                        memberType
                    } else {
                        error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
                        Type.Error
                    }
                }
                is Type.RawPtr -> TODO()
                is Type.Function,
                is Type.ParamRef,
                is Type.GenericInstance,
                Type.Byte,
                Type.Void,
                Type.Bool -> {
                    error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
                    Type.Error
                }
                is Type.Application -> {
                    inferTypeApplicationProperty(lhsType, expression.property)
                }
                is Type.Constructor -> TODO()
            }
        }
    }

    private fun inferTypeApplicationProperty(lhsType: Type.Application, property: Identifier): Type =
        when (lhsType.callee) {
            is Type.Constructor -> {
                val binder = requireNotNull(lhsType.callee.binder)
                when (val binding = ctx.resolver.resolveTypeVariable(binder.identifier)) {
                    is TypeBinding.Struct -> {
                        if (binding.declaration.typeParams == null) {
                            error(property, Diagnostic.Kind.NoSuchProperty(lhsType, property.name))
                            Type.Error
                        } else {
                            val members = typeOfStructMembers(binding.declaration)
                            val fieldType = members[property.name]
                            if (fieldType == null) {
                                error(property, Diagnostic.Kind.NoSuchProperty(lhsType, property.name))
                                Type.Error
                            } else {
                                val substitution = binding.declaration.typeParams.mapIndexed { index, it ->
                                    it.binder.location to lhsType.args.elementAtOrElse(index) { Type.Error }
                                }.toMap()

                                fieldType.applySubstitution(substitution)
                            }
                        }
                    }
                    else -> {
                        error(property, Diagnostic.Kind.NoSuchProperty(lhsType, property.name))
                        Type.Error
                    }
                }

            }
            else -> {
                error(property, Diagnostic.Kind.NoSuchProperty(lhsType, property.name))
                Type.Error
            }
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
            for (arg in expression.args) {
                applyInstantiations(arg.expression)
            }
            val typeArgs = mutableListOf<Type>()
            calleeType.typeParams?.forEach {
                val generic = requireNotNull(substitution[it.binder.location])
                val instance = genericInstantiations[generic.id]
                typeArgs.add(
                    if (instance == null) {
                        error(expression.callee, Diagnostic.Kind.UninferrableTypeParam(it.binder))
                        Type.Error
                    } else {
                        instance
                    }
                )
            }
            if (calleeType.typeParams != null) {
                typeArguments[expression] = typeArgs
            }
            return applyInstantiations(expression.location, to)
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


    private fun applyInstantiations(location: SourceLocation, type: Type): Type = when (type) {
        Type.Error,
        Type.Byte,
        Type.Void,
        is Type.ParamRef,
        Type.Bool -> type
        is Type.RawPtr -> Type.RawPtr(type.to)
        is Type.Function -> Type.Function(
            typeParams = type.typeParams,
            from = type.from.map { applyInstantiations(location, it) },
            to = applyInstantiations(location, type.to)
        )
        is Type.Struct ->
            Type.Struct(
                constructor = type.constructor,
                memberTypes = type.memberTypes.mapValues { applyInstantiations(location, it.value) }
            )
        is Type.GenericInstance -> {
            genericInstantiations[type.id] ?: type
        }
        is Type.Application -> {
            Type.Application(
                applyInstantiations(location, type.callee),
                type.args.map { applyInstantiations(location, it) }
            )
        }
        is Type.Constructor -> type
    }

    private fun applyInstantiations(expression: Expression) {
        val ty = requireNotNull(expressionTypes[expression])
        val instance = applyInstantiations(expression.location, ty)
        expressionTypes[expression] = instance
    }

    private fun checkExpression(expected: Type, expression: Expression) = when (expression) {
        else -> {
            val exprType = inferExpression(expression)
            checkAssignability(expression.location, destination = expected, source = exprType)
        }
    }

    private fun checkAssignability(location: SourceLocation, source: Type, destination: Type): Unit = when {
        source is Type.Error || destination is Type.Error -> {
        }
        source is Type.Bool && destination is Type.Bool -> {
        }
        source is Type.Byte && destination is Type.Byte -> {
        }
        source is Type.ParamRef && destination is Type.ParamRef
                && source.name.location == destination.name.location -> {
        }
        source is Type.RawPtr && destination is Type.RawPtr ->
            checkAssignability(location, source.to, destination.to)
        source is Type.Struct && destination is Type.Struct && source.constructor == destination.constructor -> {
        }
        source is Type.Constructor && destination is Type.Constructor && source.name == destination.name -> {
        }

        source is Type.Application && destination is Type.Application -> {
            checkAssignability(location, source.callee, destination.callee)
            val report = { error(location, Diagnostic.Kind.TypeNotAssignable(source, destination)) }
            if (source.args.size != destination.args.size) {
                report()
            } else {
                source.args.zip(destination.args).forEach {
                    checkAssignability(location, it.first, it.second)
                }
            }
        }
        destination is Type.GenericInstance -> {
            val destinationInstance = genericInstantiations[destination.id]
            if (destinationInstance != null) {
                checkAssignability(location, source, destinationInstance)
            } else {
                genericInstantiations[destination.id] = source
            }
        }
        else -> {
            error(location, Diagnostic.Kind.TypeNotAssignable(source, destination))
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

    private val structFieldTypes = MutableNodeMap<Declaration.Struct, Map<Name, Type>>()
    private fun declareStruct(declaration: Declaration.Struct) {
        if (binderTypes[declaration.binder] != null) {
            return
        }
        val fieldTypes = mutableMapOf<Name, Type>()
        for (member in declaration.members) {
            val exhaustive = when (member) {
                Declaration.Struct.Member.Error -> {
                }
                is Declaration.Struct.Member.Field -> {
                    val ty = inferAnnotation(member.typeAnnotation)
                    require(fieldTypes[member.binder.identifier.name] == null) { TODO("Duplicate struct field") }
                    fieldTypes[member.binder.identifier.name] = ty
                    Unit
                }
            }
        }
        val name = ctx.resolver.getQualifiedName(declaration.binder)
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
            typeParams = declaration.typeParams?.map { Type.Param(it.binder) })
        bindValue(declaration.binder, constructorType)
    }
}

private class NodeSet<T : HasLocation> {
    val set = mutableSetOf<SourceLocation>()
    fun contains(value: T): Boolean {
        return set.contains(value.location)
    }

    fun add(value: T) {
        set.add(value.location)
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

