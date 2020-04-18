package hadesc.checker

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.ValueBinding
import hadesc.types.Type
import java.util.*
import kotlin.math.min

@OptIn(ExperimentalStdlibApi::class)
class Checker(val ctx: Context) {

    private val binderTypes = NodeMap<Binder, Type>()
    private val expressionTypes = NodeMap<Expression, Type>()
    private val annotationTypes = NodeMap<TypeAnnotation, Type>()
    private val returnTypeStack = Stack<Type>()

    fun typeOfExpression(expression: Expression): Type = expressionTypes.computeIfAbsent(expression) {
        val decl = ctx.resolver.getDeclarationContaining(expression)
        checkDeclaration(decl)
        requireNotNull(expressionTypes[expression])
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

    private fun inferAnnotation(annotation: TypeAnnotation): Type {
        val type = when (annotation) {
            is TypeAnnotation.Error -> Type.Error
            is TypeAnnotation.Var -> when (annotation.name.name.text) {
                "Void" -> Type.Void
                "Bool" -> Type.Bool
                "Byte" -> Type.Byte
                else -> TODO()
            }
            is TypeAnnotation.Ptr -> Type.RawPtr(inferAnnotation(annotation.to))
        }
        annotationTypes[annotation] = type
        return type
    }

    fun isTypeEqual(t1: Type, t2: Type): Boolean {
        return t1 == t2
    }

    fun typeOfStructConstructor(declaration: Declaration.Struct): Type {
        return typeOfBinder(declaration.binder)
    }

    fun typeOfStructInstance(declaration: Declaration.Struct): Type.Struct {
        val constructorType = typeOfStructConstructor(declaration)
        require(constructorType is Type.Function)
        val instanceType = constructorType.to
        require(instanceType is Type.Struct)
        return instanceType
    }

    fun getTypeArgs(call: Expression.Call): List<Type>? {
        return null
    }

    private fun checkDeclaration(declaration: Declaration) = when (declaration) {
        is Declaration.Error -> {
        }
        is Declaration.ImportAs -> TODO()
        is Declaration.FunctionDef -> checkFunctionDef(declaration)
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.Struct -> checkStructDef(declaration)
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        val functionType = declareFunctionDef(declaration)
        withReturnType(functionType.to) {
            checkBlock(declaration.body)
        }
        require(declaration.typeParams == null)
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
            typeParams = null
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

    private fun checkValStatement(statement: Statement.Val) {
        val typeAnnotation = statement.typeAnnotation
        val type = if (typeAnnotation != null) {
            val expected = inferAnnotation(typeAnnotation)
            checkExpression(expected, statement.rhs)
            expected
        } else {
            inferExpression(statement.rhs)
        }

        bindValue(statement.binder, type)
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
                is Type.ParamRef -> TODO()
                is Type.Deferred -> TODO()
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
                is Type.Function -> TODO()
                Type.Byte,
                Type.Void,
                Type.Bool -> {
                    error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
                    Type.Error
                }
            }
        }
    }

    private fun inferCall(expression: Expression.Call): Type {
        val calleeType = inferExpression(expression.callee)
        if (calleeType is Type.Function) {
            require(calleeType.typeParams == null)

            val len = min(calleeType.from.size, expression.args.size)
            for (index in 0 until len) {
                val expected = calleeType.from[index]
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
            return calleeType.to
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

    private fun checkExpression(expected: Type, expression: Expression) = when (expression) {
        else -> {
            val exprType = inferExpression(expression)
            if (!(exprType isAssignableTo expected)) {
                error(expression, Diagnostic.Kind.TypeNotAssignable(exprType, to = expected))
            }
            Unit
        }
    }

    private infix fun Type.isAssignableTo(to: Type): Boolean = when {
        else -> this == to
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
        ctx.diagnosticReporter.report(node.location, kind)
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
        val instanceType = Type.Struct(name, fieldTypes)
        val constructorParamTypes = fieldTypes.values.toList()
        require(declaration.typeParams == null) { TODO() }
        val constructorType = Type.Function(from = constructorParamTypes, to = instanceType, typeParams = null)
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

private class NodeMap<T : HasLocation, V> {
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