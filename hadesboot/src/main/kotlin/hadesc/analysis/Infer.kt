package hadesc.analysis

import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.frontend.PropertyBinding
import hadesc.hir.TypeTransformer
import hadesc.location.HasLocation
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.types.toSubstitution
import hadesc.unit

data class InferResult(
    val expressionTypes: List<Pair<Expression, Type>>,
    val binderTypes: List<Pair<Binder, Type>>,
    val typeArgs: List<Pair<Expression, List<Type>>>,
    val propertyBindings: List<Pair<Expression.Property, PropertyBinding>>
)

fun infer(
    member: Block.Member,
    returnType: Type,
    ctx: Context
): InferResult {
    val infer = Infer(returnType, ctx)
    infer.visitBlockMember(member)
    return InferResult(
        infer.exprTypes.entries
            .map{
                it.first to infer.reduceGenericInstances(it.second)
            }.toList(),
        infer.binderTypes.entries
            .map {
                it.first to infer.reduceGenericInstances(it.second)
            }.toList(),
        infer.typeArgsOfExpression.entries
            .map { (expr, typeArgs) ->
                expr to typeArgs.map { infer.reduceGenericInstances(it) }
            }
            .toList(),
        infer.propertyBindings.entries
            .map { (expr, binding) ->
                expr to infer.reduceGenericInstances(binding)
            }
            .toList()
    )
}

private class Infer(
    private val returnType: Type,
    private val ctx: Context
) {
    val exprTypes = MutableNodeMap<Expression, Type>()
    val binderTypes = MutableNodeMap<Binder, Type>()
    val typeArgsOfExpression = MutableNodeMap<Expression, List<Type.GenericInstance>>()
    val propertyBindings = MutableNodeMap<Expression.Property, PropertyBinding>()
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
        is Statement.Return -> visitReturnStatement(statement)
        is Statement.Val -> visitValStatement(statement)
        is Statement.While -> TODO()
    }

    private fun visitReturnStatement(statement: Statement.Return) {
        if (statement.value != null) {
            checkExpression(statement.value, returnType)
        } else if (returnType != Type.Void) {
            reportError(statement, Diagnostic.Kind.MissingReturnValue)
        }
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

    fun inferOrCheckCall(expression: Expression.Call, expectedType: Type? = null): Type {
        val calleeType = inferExpression(expression.callee)
        fun reportNotCallable(): Type {
            return errorType(expression.callee, Diagnostic.Kind.TypeNotCallable(calleeType))
        }
        return when (calleeType) {
            is Type.Ptr -> when (calleeType.to) {
                is Type.Function -> {
                    val fnType = calleeType.to
                    checkFunctionArgs(fnType, expression)
                    fnType.to
                }
                else -> reportNotCallable()
            }
            is Type.TypeFunction -> {
                if (calleeType.body !is Type.Ptr || calleeType.body.to !is Type.Function) {
                    return reportNotCallable()
                }
                val typeParams = calleeType.params
                val typeArgs = calleeType.params.map { typeAnalyzer.makeGenericInstance(it.binder) }
                val substitution = typeParams.zip(typeArgs).associate { it.first.binder.location to it.second }.toSubstitution()
                val appliedCalleeType = calleeType.body.to.applySubstitution(substitution)
                check(appliedCalleeType is Type.Function)
                checkFunctionArgs(appliedCalleeType, expression)
                check(typeArgsOfExpression[expression.callee] === null)
                typeArgsOfExpression[expression.callee] = typeArgs
                appliedCalleeType.to
            }
            else -> {
                reportNotCallable()
            }
        }
    }

    private fun checkFunctionArgs(fnType: Type.Function, expression: Expression.Call) {
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
            unify(at = expression, expected = expected, actual = actualType)
            unit
        }
    }

    private fun unify(at: HasLocation, expected: Type, actual: Type) {
        if (
            !typeAnalyzer.isTypeAssignableTo(source = actual, destination = expected) &&
            expected !is Type.Error
        ) {
            reportError(at, Diagnostic.Kind.TypeNotAssignable(source = actual, destination = expected))
        }
    }

    fun inferExpression(expression: Expression): Type =
        exprTypes.getOrPut(expression) {
            check(exprTypes[expression] == null)
            inferExpressionWorker(expression)
        }

    fun inferExpressionWorker(expression: Expression): Type = when(expression) {
        is Expression.AddressOf -> TODO()
        is Expression.AddressOfMut -> inferAddressOfMut(expression)
        is Expression.ArrayIndex -> TODO()
        is Expression.ArrayLiteral -> TODO()
        is Expression.As -> inferAsExpression(expression)
        is Expression.BinaryOperation -> inferBinOp(expression)
        is Expression.BlockExpression -> TODO()
        is Expression.BoolLiteral -> Type.Bool
        is Expression.ByteCharLiteral -> Type.u8
        is Expression.ByteString -> Type.constBytePtr
        is Expression.Call -> inferOrCheckCall(expression, expectedType = null)
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

    private fun inferAddressOfMut(expression: Expression.AddressOfMut): Type {
        val valueType = inferExpression(expression.expression)

        if (!isLValue(expression.expression)) {
            reportError(expression.expression, Diagnostic.Kind.NotAnAddressableValue)
            if (!isMutableLValue(expression.expression)) {
                reportError(expression.expression, Diagnostic.Kind.ValNotMutable)
            }
        }


        return Type.Ptr(valueType, isMutable = true)
    }

    private fun isLValue(expression: Expression): Boolean {
        return getLValueInfo(expression) != null
    }

    private fun isMutableLValue(expression: Expression): Boolean {
        return getLValueInfo(expression)?.isMutable == true
    }

    private fun getLValueInfo(expression: Expression): LValueInfo? = when(expression) {
        is Expression.Var -> when (val binding = ctx.resolver.resolve(expression.name)) {
            is Binding.ValBinding -> LValueInfo(isMutable = binding.statement.isMutable)
            else -> null
        }
        else -> null
    }

    private fun inferPropertyExpression(expression: Expression.Property): Type {
        val propertyBinding: PropertyBinding? =
            inferModuleProperty(expression)
            ?: inferStructFieldProperty(expression)

        return if (propertyBinding != null) {
            check(propertyBindings[expression] == null)
            propertyBindings[expression] = propertyBinding
            typeOfPropertyBinding(propertyBinding)
        } else {
            val kind = if (propertyBinding is PropertyBinding.Global) {
                Diagnostic.Kind.NoSuchMember
            } else {
                Diagnostic.Kind.NoSuchProperty(inferExpression(expression.lhs), expression.property.name)
            }
            errorType(expression.property, kind)
        }

    }

    private fun inferStructFieldProperty(expression: Expression.Property): PropertyBinding.StructField? {
        return when (val lhsType = inferExpression(expression.lhs)) {
            is Type.Application -> TODO()
            is Type.Constructor -> {
                val structDecl = ctx.resolver.resolveDeclaration(lhsType.name) ?: return null
                if (structDecl !is Declaration.Struct) {
                    return null
                }
                val field = structDecl.getField(expression.property.name) ?: return null
                val fieldIndex = structDecl.getFieldIndex(expression.property.name)
                PropertyBinding.StructField(structDecl, fieldIndex, field.typeAnnotation.toType())
            }
            else -> null
        }
    }

    private fun inferModuleProperty(expression: Expression.Property): PropertyBinding.Global? {
        return when (expression.lhs) {
            is Expression.Property -> TODO()
            is Expression.Var -> {
                val sourceFile = ctx.resolver.resolveModuleAlias(expression.lhs.name)
                if (sourceFile == null) {
                    null
                } else {
                    val binding = ctx.resolver.findInSourceFile(expression.property.name, sourceFile)
                    if (binding == null) {
                        reportError(expression.property, Diagnostic.Kind.NoSuchMember)
                        null
                    } else {
                        PropertyBinding.Global(binding)
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
        is Binding.Struct -> typeOfStructConstructor(binding.declaration)
        is Binding.ValBinding -> typeOfValBinding(binding)
        is Binding.WhenArm -> TODO()
    }

    private fun typeOfStructConstructor(declaration: Declaration.Struct): Type {
        check(declaration.typeParams == null)
        val from = declaration.members.map {
            when (it) {
                is Declaration.Struct.Member.Field -> {
                    it.typeAnnotation.toType()
                }
            }
        }

        val to = Type.Constructor(ctx.resolver.qualifiedStructName(declaration))

        val fnType = Type.Function(
            from,
            traitRequirements = null,
            to
        )
        return Type.Ptr(fnType, isMutable = false)
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
        val fnPtrType = Type.Ptr(fnType, isMutable = false)
        return if (declaration.signature.typeParams != null) {
            return Type.TypeFunction(
                params = declaration.signature.typeParams.map { Type.Param(it.binder) },
                fnPtrType
            )
        } else {
            fnPtrType
        }

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

    fun reduceGenericInstances(type: Type): Type {
        return object: TypeTransformer {
            override fun lowerGenericInstance(type: Type.GenericInstance): Type {
                return typeAnalyzer.getInstantiatedType(type) ?: type
            }
        }.lowerType(type)
    }

    fun reduceGenericInstances(binding: PropertyBinding): PropertyBinding = when(binding) {
        is PropertyBinding.ExtensionDef -> binding.copy(type = reduceGenericInstances(binding.type))
        is PropertyBinding.Global -> binding
        is PropertyBinding.SealedTypeCaseConstructor -> binding.copy(type = reduceGenericInstances(binding.type))
        is PropertyBinding.StructField -> binding.copy(type = reduceGenericInstances(binding.type))
        is PropertyBinding.StructFieldPointer -> binding.copy(type = reduceGenericInstances(binding.type))
        is PropertyBinding.TraitFunctionRef -> binding.copy(type = reduceGenericInstances(binding.type))
        is PropertyBinding.WhenCaseFieldRef -> binding.copy(type = reduceGenericInstances(binding.type))
        is PropertyBinding.WhereParamRef -> binding.copy(type = reduceGenericInstances(binding.type))
    }

    fun typeOfPropertyBinding(binding: PropertyBinding): Type = when(binding) {
        is PropertyBinding.Global -> typeOfBinding(binding.binding) ?: Type.Error(binding.binding.binder.location)
        is PropertyBinding.ExtensionDef -> binding.type
        is PropertyBinding.SealedTypeCaseConstructor -> binding.type
        is PropertyBinding.StructField -> binding.type
        is PropertyBinding.StructFieldPointer -> binding.type
        is PropertyBinding.TraitFunctionRef -> binding.type
        is PropertyBinding.WhenCaseFieldRef -> binding.type
        is PropertyBinding.WhereParamRef -> binding.type
    }

}

data class LValueInfo(val isMutable: Boolean)