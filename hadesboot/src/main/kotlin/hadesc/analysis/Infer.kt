package hadesc.analysis

import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.unit

data class InferResult(
    val expressionTypes: List<Pair<Expression, Type>>,
    val binderTypes: List<Pair<Binder, Type>>,
)

fun infer(
    member: Block.Member,
    returnType: Type,
    ctx: Context
): InferResult {
    val infer = Infer(returnType, ctx)
    infer.visitBlockMember(member)
    return InferResult(infer.exprTypes.entries.toList(), infer.binderTypes.entries.toList())
}

private class Infer(
    private val returnType: Type,
    private val ctx: Context
) {
    val exprTypes = MutableNodeMap<Expression, Type>()
    val binderTypes = MutableNodeMap<Binder, Type>()
    private val typeAnalyzer = TypeAnalyzer()

    fun visitBlockMember(member: Block.Member): Unit = when(member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            unit
        }
        is Block.Member.Statement -> visitStatement(member.statement)
    }

    private fun visitStatement(statement: Statement): Unit = when (statement) {
        is Statement.Defer -> TODO()
        is Statement.Error -> TODO()
        is Statement.If -> visitIfStatement(statement)
        is Statement.LocalAssignment -> visitLocalAssignment(statement)
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Return -> TODO()
        is Statement.Val -> visitValStatement(statement)
        is Statement.While -> TODO()
    }

    private fun visitLocalAssignment(statement: Statement.LocalAssignment) {
        when (val binding = ctx.resolver.resolve(statement.name)) {
            is Binding.ValBinding -> {
                if (!binding.statement.isMutable) {
                    reportError(statement.name, Diagnostic.Kind.ValNotMutable)
                }
                val expectedType = typeOfValBinding(binding)
                    ?: errorType(statement.name, Diagnostic.Kind.UseBeforeDefinition)
                checkExpression(statement.value, expectedType)
            }
            else -> {
                reportError(statement.name, Diagnostic.Kind.ValNotMutable)
            }
        }
    }

    private fun visitIfStatement(statement: Statement.If) {
        checkExpression(statement.condition, Type.Bool)
        visitBlock(statement.ifTrue)
        statement.ifFalse?.let { visitBlock(it) }
    }

    private fun visitBlock(block: Block) {
        for (member in block.members) {
            visitBlockMember(member)
        }
    }

    private fun visitValStatement(statement: Statement.Val) {
        val annotated = statement.typeAnnotation?.toType()
        val valType = if (annotated != null) {
            checkExpression(statement.rhs, annotated)
            annotated
        } else {
            inferExpression(statement.rhs)
        }
        check(binderTypes[statement.binder] == null)
        binderTypes[statement.binder] = valType
    }

    private fun TypeAnnotation.toType(): Type {
        return ctx.analyzer.annotationToType(this)
    }

    fun inferBinOp(expression: Expression.BinaryOperation): Type {
        TODO()
    }

    fun inferCall(expression: Expression.Call): Type {
        val calleeType = inferExpression(expression.callee)
        fun reportNotCallable(): Type {
            return errorType(expression.callee, Diagnostic.Kind.TypeNotCallable(calleeType))
        }
        return when (calleeType) {
            is Type.Ptr -> when (calleeType.to) {
                is Type.Function -> {
                    val fnType = calleeType.to
                    if (fnType.from.size != expression.args.size) {
                        if (fnType.from.size > expression.args.size) {
                            reportError(expression.callee, Diagnostic.Kind.TooManyArgs(required = fnType.from.size))
                        } else {
                            reportError(expression.callee, Diagnostic.Kind.MissingArgs(required = fnType.from.size))
                        }
                    }
                    fnType.from.zip(expression.args).forEach { (expected, arg) ->
                        checkExpression(arg.expression, expected)
                    }
                    fnType.to
                }
                else -> reportNotCallable()
            }
            else -> {
                reportNotCallable()
            }
        }
    }

    private fun checkExpression(expression: Expression, expected: Type) {
        checkExpressionWorker(expression, expected)
        exprTypes[expression] = expected
    }
    private fun checkExpressionWorker(expression: Expression, expected: Type): Unit = when(expression) {
        is Expression.IntLiteral -> {
            if (!expected.isIntegral() && expected !is Type.FloatingPoint) {
                reportError(expression, Diagnostic.Kind.NotAnIntegralValue)
            }
            unit
        }
        else -> {
            val actualType = inferExpression(expression)
            if (
                !typeAnalyzer.isTypeAssignableTo(source = actualType, destination = expected) &&
                    expected !is Type.Error
            ) {
                reportError(expression, Diagnostic.Kind.TypeNotAssignable(source = actualType, destination = expected))
            }
            unit
        }
    }

    fun inferExpression(expression: Expression): Type =
        exprTypes.getOrPut(expression) {
            inferExpressionWorker(expression)
        }

    fun inferExpressionWorker(expression: Expression): Type = when(expression) {
        is Expression.AddressOf -> TODO()
        is Expression.AddressOfMut -> TODO()
        is Expression.ArrayIndex -> TODO()
        is Expression.ArrayLiteral -> TODO()
        is Expression.As -> inferAsExpression(expression)
        is Expression.BinaryOperation -> inferBinOp(expression)
        is Expression.BlockExpression -> TODO()
        is Expression.BoolLiteral -> Type.Bool
        is Expression.ByteCharLiteral -> Type.u8
        is Expression.ByteString -> Type.constBytePtr
        is Expression.Call -> inferCall(expression)
        is Expression.Closure -> TODO()
        is Expression.Deref -> TODO()
        is Expression.Error -> TODO()
        is Expression.If -> TODO()
        is Expression.IntLiteral -> Type.isize
        is Expression.Intrinsic -> TODO()
        is Expression.Match -> TODO()
        is Expression.Not -> inferNotExpression(expression)
        is Expression.NullPtr -> TODO()
        is Expression.PointerCast -> TODO()
        is Expression.Property -> inferPropertyExpression(expression)
        is Expression.SizeOf -> TODO()
        is Expression.This -> TODO()
        is Expression.TypeApplication -> TODO()
        is Expression.UnaryMinus -> TODO()
        is Expression.UnsafeCast -> TODO()
        is Expression.Var -> inferVarExpression(expression)
        is Expression.When -> TODO()
    }

    private fun inferPropertyExpression(expression: Expression.Property): Type {
        return inferModuleProperty(expression)
            ?: TODO()
    }

    private fun inferModuleProperty(expression: Expression.Property): Type? {
        return when (expression.lhs) {
            is Expression.Property -> TODO()
            is Expression.Var -> {
                val sourceFile = ctx.resolver.resolveModuleAlias(expression.lhs.name)
                if (sourceFile == null) {
                    null
                } else {
                    val binding = ctx.resolver.findInSourceFile(expression.property.name, sourceFile)
                    if (binding == null) {
                        errorType(expression.property, Diagnostic.Kind.NoSuchMember)
                    } else {
                        checkNotNull(typeOfBinding(binding)) {
                            "ModuleProperty binding type should not be null"
                        }
                    }
                }
            }
            else -> null
        }
    }

    private fun inferNotExpression(expression: Expression.Not): Type {
        checkExpression(expression.expression, Type.Bool)
        return Type.Bool
    }

    private fun inferAsExpression(expression: Expression.As): Type {
        val expected = expression.rhs.toType()
        checkExpression(expression.lhs, expected)
        return expected
    }


    private fun inferVarExpression(expression: Expression.Var): Type {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> {
                errorType(expression, Diagnostic.Kind.UnboundVariable(expression.name.name))
            }
            else -> typeOfBinding(binding)
                ?: errorType(expression.location, Diagnostic.Kind.UseBeforeDefinition)
        }
    }

    private fun typeOfBinding(binding: Binding): Type? = when(binding) {
        is Binding.ClosureParam -> TODO()
        is Binding.ExternConst -> TODO()
        is Binding.ExternFunction -> {
            Type.Ptr(
                Type.Function(
                    from = binding.declaration.paramTypes.map { it.toType() },
                    to = binding.declaration.returnType.toType(),
                    traitRequirements = null
                ),
                isMutable = false
            )
        }
        is Binding.FunctionParam -> {
            binding.param.annotation?.toType()
                ?: typeAnalyzer.makeGenericInstance(binding.param.binder)
        }
        is Binding.GlobalConst -> TODO()
        is Binding.GlobalFunction -> typeOfGlobalFunction(binding.declaration)
        is Binding.SealedType -> TODO()
        is Binding.Struct -> TODO()
        is Binding.ValBinding -> typeOfValBinding(binding)
        is Binding.WhenArm -> TODO()
    }

    private fun typeOfGlobalFunction(declaration: Declaration.FunctionDef): Type {
        val traitRequirements: List<TraitRequirement>? = if (declaration.signature.whereClause == null) {
            null
        } else {
            TODO()
        }
        val fnType = Type.Function(
            from = declaration.params.map { it.annotation?.toType() ?: Type.Error(it.location) },
            to = declaration.signature.returnType.toType(),
            traitRequirements = traitRequirements
        )
        if (declaration.typeParams != null) {
            TODO()
        }

        return Type.Ptr(fnType, isMutable = false)
    }

    private fun typeOfValBinding(binding: Binding.ValBinding): Type? {
        return binderTypes[binding.binder]
            ?: ctx.analyzer.typeOfBinder(binding.binder)
    }

    private fun errorType(node: HasLocation, diagnostic: Diagnostic.Kind): Type {
        reportError(node, diagnostic)
        return Type.Error(node.location)
    }

    private fun reportError(node: HasLocation, diagnostic: Diagnostic.Kind) {
        val location = node.location
        ctx.diagnosticReporter.report(location, diagnostic)
    }

}