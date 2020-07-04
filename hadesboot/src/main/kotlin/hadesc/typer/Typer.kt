package hadesc.typer

import hadesc.ast.*
import hadesc.context.Context
import hadesc.ir.BinaryOperator
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.Binding
import hadesc.resolver.TypeBinding
import hadesc.types.Type
import java.util.*
import kotlin.math.min

@OptIn(ExperimentalStdlibApi::class)
class Typer(
        private val ctx: Context
) {
    private val returnTypeStack = Stack<Type>()

    fun getPropertyBinding(expression: Expression.Property): PropertyBinding {
        TODO()
    }

    private val typeOfExpressionCache = MutableNodeMap<Expression, Type>()
    fun typeOfExpression(expression: Expression): Type {
        val cached = typeOfExpressionCache[expression]
        if (cached != null) {
            return cached
        }
        val def = requireNotNull(ctx.resolver.getEnclosingFunction(expression))
        visitFunction(def)
        return requireNotNull(typeOfExpressionCache[expression])

    }

    private fun visitFunction(def: Declaration.FunctionDef) {
        val returnType = annotationToType(def.signature.returnType)
        returnTypeStack.push(returnType)
        visitBlock(def.body)
        returnTypeStack.pop()

    }

    private fun visitBlock(block: Block) {
        for (member in block.members) {
            visitBlockMember(member)
        }
    }

    private fun visitBlockMember(member: Block.Member): Unit = when(member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> visitStatement(member.statement)
    }

    private fun visitStatement(statement: Statement): Unit = when(statement) {
        is Statement.Return -> visitReturnStatement(statement)
        is Statement.Val -> visitValStatement(statement)
        is Statement.While -> TODO()
        is Statement.If -> TODO()
        is Statement.LocalAssignment -> TODO()
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Defer -> TODO()
        is Statement.Error -> TODO()
    }

    private fun visitReturnStatement(statement: Statement.Return) {
        if (statement.value != null) {
            val returnType = returnTypeStack.peek()
            checkExpression(statement.value, returnType)
        }
    }

    private fun visitValStatement(statement: Statement.Val) {
        val expectedType = statement.typeAnnotation?.let { annotationToType(it) }
        if (expectedType != null) {
            checkExpression(statement.rhs, expectedType)
        } else {
            inferExpression(statement.rhs)
        }
    }

    private fun checkExpression(expression: Expression, expectedType: Type) = when(expression) {
        else -> {
            inferExpression(expression)
        }
    }

    private fun inferExpression(expression: Expression): Type {
        val type = when (expression) {
            is Expression.Error -> Type.Error
            is Expression.Var -> inferVarExpresion(expression)
            is Expression.Call -> inferCallExpression(expression)
            is Expression.Property -> TODO()
            is Expression.ByteString -> Type.Ptr(Type.Byte, isMutable = false)
            is Expression.BoolLiteral -> TODO()
            is Expression.This -> TODO()
            is Expression.NullPtr -> TODO()
            is Expression.IntLiteral -> TODO()
            is Expression.Not -> TODO()
            is Expression.BinaryOperation -> TODO()
            is Expression.SizeOf -> TODO()
            is Expression.AddressOf -> TODO()
            is Expression.AddressOfMut -> TODO()
            is Expression.Deref -> TODO()
            is Expression.PointerCast -> TODO()
            is Expression.If -> TODO()
            is Expression.TypeApplication -> TODO()
            is Expression.Match -> TODO()
            is Expression.New -> TODO()
        }

        typeOfExpressionCache[expression] = type
        return type

    }

    private fun inferVarExpresion(expression: Expression.Var): Type {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> Type.Error
            else -> typeOfBinding(binding)
        }
    }

    private fun typeOfBinding(binding: Binding): Type = when(binding) {
        is Binding.GlobalFunction -> typeOfGlobalFunctionRef(binding.declaration)
        is Binding.ExternFunction -> typeOfExternFunctionRef(binding.declaration)
        is Binding.FunctionParam -> typeOfParam(binding.declaration, binding.index)
        is Binding.ValBinding -> TODO()
        is Binding.Struct -> TODO()
        is Binding.GlobalConst -> TODO()
        is Binding.EnumCaseConstructor -> TODO()
        is Binding.Pattern -> TODO()
    }

    private fun typeOfParam(declaration: Declaration.FunctionDef, paramIndex: Int): Type {
        val annotation = declaration.params[paramIndex].annotation
        return if (annotation == null) {
            Type.Error
        } else {
            annotationToType(annotation)
        }
    }

    private fun typeOfExternFunctionRef(declaration: Declaration.ExternFunctionDef): Type {
        return Type.Ptr(
                to = Type.Function(
                        constraints = emptyList(),
                        receiver = null,
                        from = declaration.paramTypes.map { annotationToType(it) },
                        to = annotationToType(declaration.returnType)
                ),
                isMutable = false
        )
    }

    private fun typeOfGlobalFunctionRef(declaration: Declaration.FunctionDef): Type {
        val functionType = Type.Function(
                from = declaration.params.map { param ->
                    param.annotation?.let { annotationToType(it) } ?: Type.Error },
                to = annotationToType(declaration.signature.returnType),
                receiver = declaration.thisParam?.let { annotationToType(it.annotation) }
        )
        val typeParams = declaration.typeParams
        val type = if (typeParams != null) {
            Type.TypeFunction(
                    params = typeParams.map { Type.Param(it.binder) },
                    body = functionType
            )
        } else functionType
        return Type.Ptr(
                to = type,
                isMutable = false
        )
    }

    data class FunctionTypeComponents(
            val from: List<Type>,
            val to: Type,
            val typeParams: List<Type.Param>?
    )
    private fun inferCallExpression(expression: Expression.Call): Type {
        return inferCallLikeExpression(inferExpression(expression.callee), expression.args)
    }

    private fun inferCallLikeExpression(
            calleeType: Type,
            args: List<Arg>
    ): Type {
        val functionType = getFunctionTypeComponents(calleeType)
        return if (functionType == null) {
            for (arg in args) {
                inferExpression(arg.expression)
            }
            Type.Error
        } else {
            checkCallArgs(functionType, args)
            functionType.to
        }
    }

    private fun checkCallArgs(functionType: FunctionTypeComponents, args: List<Arg>) {
        val length = min(functionType.from.size, args.size)
        for (i in 0 until length) {
            val expectedType = functionType.from[i]
            val arg = args[i]
            checkExpression(arg.expression, expectedType)
        }
        for (arg in args.drop(length)) {
            inferExpression(arg.expression)
        }
    }

    private fun getFunctionTypeComponents(type: Type): FunctionTypeComponents? {
        return when (type) {
            is Type.Ptr -> when (type.to) {
                is Type.Function -> {
                    FunctionTypeComponents(
                            from = type.to.from,
                            to = type.to.to,
                            typeParams = null
                    )
                }
                else -> null
            }
            else -> null
        }
    }

    fun getTypeArgs(expression: Expression.Call): List<Type>? {
        return null // FIXME
    }

    fun annotationToType(annotation: TypeAnnotation): Type = when(annotation) {
        is TypeAnnotation.Error -> Type.Error
        is TypeAnnotation.Var -> varAnnotationToType(annotation)
        is TypeAnnotation.Ptr -> ptrAnnotationToType(annotation)
        is TypeAnnotation.MutPtr -> TODO()
        is TypeAnnotation.Application -> TODO()
        is TypeAnnotation.Qualified -> TODO()
        is TypeAnnotation.Function -> TODO()
        is TypeAnnotation.This -> TODO()
        is TypeAnnotation.Union -> TODO()
    }

    private fun ptrAnnotationToType(annotation: TypeAnnotation.Ptr): Type {
        return Type.Ptr(
                to = annotationToType(annotation.to),
                isMutable = false
        )
    }

    private fun varAnnotationToType(annotation: TypeAnnotation.Var): Type {
        return when (ctx.resolver.resolveTypeVariable(annotation.name)) {
            is TypeBinding.Struct -> TODO()
            is TypeBinding.TypeParam -> TODO()
            is TypeBinding.Enum -> TODO()
            is TypeBinding.TypeAlias -> TODO()
            null -> {
                when (annotation.name.name.text) {
                    "Int" -> Type.CInt
                    "Bool" -> Type.Bool
                    "Byte" -> Type.Byte
                    "Size" -> Type.Size
                    "Double" -> Type.Double
                    "Void" -> Type.Void
                    else -> Type.Error
                }
            }
        }
    }

    fun typeOfBinder(binder: Binder): Type = when (val binding = ctx.resolver.resolve(binder.identifier)) {
        null -> Type.Error
        else -> typeOfBinding(binding)
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

typealias op = BinaryOperator

val BIN_OP_RULES: Map<Pair<op, Type>, Pair<Type, Type>> = mapOf(
        (op.PLUS to Type.CInt) to (Type.CInt to Type.CInt),
        (op.MINUS to Type.CInt) to (Type.CInt to Type.CInt),
        (op.TIMES to Type.CInt) to (Type.CInt to Type.CInt),

        (op.GREATER_THAN_EQUAL to Type.CInt) to (Type.CInt to Type.Bool),
        (op.LESS_THAN_EQUAL to Type.CInt) to (Type.CInt to Type.Bool),
        (op.GREATER_THAN to Type.CInt) to (Type.CInt to Type.Bool),
        (op.LESS_THAN to Type.CInt) to (Type.CInt to Type.Bool),

        (op.PLUS to Type.Size) to (Type.Size to Type.Size),
        (op.MINUS to Type.Size) to (Type.Size to Type.Size),
        (op.TIMES to Type.Size) to (Type.Size to Type.Size),

        (op.GREATER_THAN_EQUAL to Type.Size) to (Type.Size to Type.Bool),
        (op.LESS_THAN_EQUAL to Type.Size) to (Type.Size to Type.Bool),
        (op.GREATER_THAN to Type.Size) to (Type.Size to Type.Bool),
        (op.LESS_THAN to Type.Size) to (Type.Size to Type.Bool),

        (op.AND to Type.Bool) to (Type.Bool to Type.Bool),
        (op.OR to Type.Bool) to (Type.Bool to Type.Bool)
)

enum class Variance {
    INVARIANCE
}
