package hadesc.analysis

import hadesc.Name
import hadesc.analysis.tc.Env
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.IdGenCtx
import hadesc.context.NamingCtx
import hadesc.context.ResolverCtx
import hadesc.exhaustive
import hadesc.frontend.PropertyBinding
import hadesc.hir.BinaryOperator
import hadesc.hir.TypeTransformer
import hadesc.hir.TypeVisitor
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.Binding
import hadesc.resolver.TypeBinding
import hadesc.types.*
import hadesc.unit
import java.util.*
import java.util.Collections.singletonList
import kotlin.math.min

val ARITHMETIC_OPERATORS = setOf(
    BinaryOperator.MINUS,
    BinaryOperator.PLUS,
    BinaryOperator.TIMES,
    BinaryOperator.DIV,
    BinaryOperator.REM,
)
class Analyzer<Ctx>(
    private val ctx: Ctx
) where
    Ctx: ResolverCtx,
    Ctx: IdGenCtx,
    Ctx: NamingCtx
{
    private val typeAnalyzer = TypeAnalyzer()
    private val returnTypeStack = Stack<Type?>()
    private val astConv = ASTConv(ctx.resolver)
    private var env: Env = Env.empty

    fun run(sourceFiles: List<SourceFile>): PostAnalysisContext {
        sourceFiles.forEach { sourceFile ->
            env = Env.ofSourceFile(
                sourceFile,
                ctx.resolver,
                parent = env,
                lowerType = { annotationToType(it) }
            )
            sourceFile.declarations.forEach { visitDeclaration(it) }
            val parent = env.parent
            checkNotNull(parent)
            check(parent === Env.empty)
            env = parent
        }

        return object : PostAnalysisContext {
            override val Expression.type: Type
                get() = checkNotNull(typeOfExpressionCache[this]) {
                    "Expression type not computed for $this"
                }
            override val TypeAnnotation.type: Type
                get() = checkNotNull(astConv.annotationToTypeCache[this]) {
                    "Type not computed for $this"
                }

            override val Expression.Property.binding: PropertyBinding
                get() {
                    check(this in propertyBindingCache) {
                        "Property binding not computed for $this"
                    }
                    return checkNotNull(propertyBindingCache[this]) {
                        "Property binding not computed for $this"
                    }
                }

            override val Expression.Property.bindingOrNull: PropertyBinding?
                get() {
                    check(this in propertyBindingCache) {
                        "Property binding not computed for $this"
                    }
                    return propertyBindingCache[this]
                }
        }
    }

    private val propertyBindingCache = MutableNodeMap<Expression.Property, PropertyBinding?>()
    private fun resolvePropertyBinding(expression: Expression.Property): PropertyBinding? = propertyBindingCache.getOrPut(expression) {
        val modulePropertyBinding = ctx.resolver.resolveModuleProperty(expression)
        if (modulePropertyBinding != null) {
            return@getOrPut PropertyBinding.Global(modulePropertyBinding)
        }

        val enumCaseConstructorBinding = resolveEnumConstructorBinding(expression)
        if (enumCaseConstructorBinding != null) {
            return@getOrPut enumCaseConstructorBinding
        }

        val fieldBinding = resolveStructFieldBinding(expression)
        if (fieldBinding != null) {
            return@getOrPut fieldBinding
        }

        val elementPointerBinding = resolveElementPointerBinding(expression)
        if (elementPointerBinding != null) {
            return@getOrPut elementPointerBinding
        }

        val extensionBinding = resolveExtensionBinding(expression)
        if (extensionBinding != null) {
            return@getOrPut extensionBinding
        }
        inferExpression(expression.lhs)
        null
    }

    private fun resolveEnumConstructorBinding(expression: Expression.Property): PropertyBinding.EnumTypeCaseConstructor? {
        if (expression.lhs !is Expression.Var) {
            return null
        }
        val binding = ctx.resolver.resolve(expression.lhs.name)
        if (binding !is Binding.Enum) return null
        val case = binding.declaration.cases.find { it.name.identifier.name == expression.property.name }
            ?: return null
        val type = typeOfEnumCase(binding.declaration, case)
        return PropertyBinding.EnumTypeCaseConstructor(binding.declaration, case, type)
    }

    private fun typeOfEnumCase(
        declaration: Declaration.Enum,
        case: Declaration.Enum.Case
    ): Type {
        val typeConstructor = Type.Constructor(ctx.resolver.qualifiedName(declaration.name))
        val instanceType = if (declaration.typeParams == null) {
            typeConstructor
        } else {
            Type.Application(
                typeConstructor,
                declaration.typeParams.map { Type.Param(it.binder) }
            )
        }

        val withArgs = if (case.params == null) {
            instanceType
        } else {
            Type.FunctionPtr(
                from = case.params.map { annotationToType(it.annotation) },
                to = instanceType,
            )
        }
        return if (declaration.typeParams == null) {
            withArgs
        } else {
            Type.ForAll(
                params = declaration.makeTypeParams(),
                body = withArgs
            )
        }
    }

    private fun resolveExtensionBinding(expression: Expression.Property): PropertyBinding.ExtensionDef? {
        for (extensionDef in ctx.resolver.extensionDefsInScope(expression)) {
            if (null == extensionDef.declarations.filterIsInstance<Declaration.FunctionDef>().find { it.name.identifier.name == expression.property.name }) {
                continue
            }
            if (hasExtensionMethodForType(extensionDef, expression.property.name, typeOfExpression(expression.lhs))) {
                val binding = findExtensionMethodBinding(extensionDef, expression)
                if (binding != null) {
                    return binding
                }
            }
        }
        return null
    }

    private fun findExtensionMethodBinding(extensionDef: Declaration.ExtensionDef, expression: Expression.Property): PropertyBinding.ExtensionDef? {
        var index = -1
        @Suppress("LoopWithTooManyJumpStatements")
        for (functionDef in extensionDef.functionDefs) {
            index++
            if (functionDef.signature.thisParamFlags == null) {
                continue
            }
            if (expression.property.name != functionDef.name.identifier.name) {
                continue
            }

            val thisType =
                instantiateTypeAndSubstitution(annotationToType(extensionDef.forType), extensionDef.makeTypeParams()).first
            val expectedThisType = if (functionDef.signature.thisParamFlags.isPointer) {
                Type.Ptr(
                    thisType,
                    isMutable = functionDef.signature.thisParamFlags.isMutable
                )
            } else {
                thisType
            }
            if (!isTypeAssignableTo(
                    source = inferExpression(expression.lhs),
                    destination = expectedThisType
                )
            ) {
                continue
            }
            val receiverType = if (functionDef.signature.thisParamFlags.isPointer) {
                Type.Ptr(
                    annotationToType(extensionDef.forType),
                    isMutable = functionDef.signature.thisParamFlags.isMutable
                )
            } else {
                annotationToType(extensionDef.forType)
            }
            var methodType: Type = Type.FunctionPtr(
                from = listOf(receiverType) + functionDef.params.mapIndexed { paramIndex, _ ->
                    typeOfParam(functionDef, paramIndex)
                },
                to = annotationToType(functionDef.signature.returnType),
            )
            if (extensionDef.typeParams != null || functionDef.typeParams != null) {
                methodType = Type.ForAll(
                    extensionDef.makeTypeParams() + functionDef.makeTypeParams(),
                    methodType
                )
            }
            return PropertyBinding.ExtensionDef(
                extensionDef,
                index,
                methodType
            )
        }
        return null
    }

    private fun instantiateTypeAndSubstitution(
        type: Type,
        typeParams: List<Type.Param>?
    ): Pair<Type, Substitution> {
        if (typeParams == null) return type to emptySubstitution()
        val substitution = instantiateSubstitution(typeParams)
        return type.applySubstitution(substitution) to substitution
    }

    private fun hasExtensionMethodForType(extensionDef: Declaration.ExtensionDef, methodName: Name, type: Type): Boolean {
        val hasRequiredMethodName = extensionDef.declarations.filterIsInstance<Declaration.FunctionDef>()
            .any { it.name.name == methodName }
        if (!hasRequiredMethodName) return false
        val forType = annotationToType(extensionDef.forType)
        val valueAssignmentSubstitution = extensionDef.typeParams?.associate {
            it.binder.id to typeAnalyzer.makeGenericInstance(it.binder)
        }?.toSubstitution() ?: emptySubstitution()
        val pointerAssignmentSubstitution = extensionDef.typeParams?.associate {
            it.binder.id to typeAnalyzer.makeGenericInstance(
                it.binder
            )
        }?.toSubstitution() ?: emptySubstitution()
        val isValueAssignable = isTypeAssignableTo(
            source = type,
            destination = forType.applySubstitution(valueAssignmentSubstitution)
        )
        val isPointerAssignable = isTypeAssignableTo(
            source = type,
            destination = Type.Ptr(forType.applySubstitution(pointerAssignmentSubstitution), isMutable = false)
        )

        return isValueAssignable || isPointerAssignable
    }

    private fun resolveElementPointerBinding(expression: Expression.Property): PropertyBinding.StructPointerFieldLoad? {
        val lhsType = inferExpression(expression.lhs)
        if (lhsType !is Type.Ptr) {
            return null
        }
        val fieldBinding = resolveStructFieldBinding(lhsType.to, expression.property) ?: return null
        return PropertyBinding.StructPointerFieldLoad(
            fieldBinding.structDecl,
            fieldBinding.memberIndex,
            type = fieldBinding.type
        )
    }

    private fun resolveStructFieldBinding(lhsType: Type, property: Identifier): PropertyBinding.StructField? {
        val structDecl = getStructDeclOfType(lhsType)
        return if (structDecl == null) {
            null
        } else {
            val index = structDecl.members.indexOfFirst {
                it is Declaration.Struct.Member.Field &&
                    it.binder.identifier.name == property.name
            }
            if (index > -1) {
                val field = structDecl.members[index]
                require(field is Declaration.Struct.Member.Field)
                val typeArgs = if (lhsType is Type.Application) {
                    lhsType.args
                } else {
                    emptyList()
                }
                val substitution = structDecl.typeParams?.zip(typeArgs)
                    ?.toSubstitution()
                    ?: emptySubstitution()
                val fieldType = annotationToType(field.typeAnnotation).applySubstitution(substitution)
                isTypeAssignableTo(lhsType, fieldType)
                PropertyBinding.StructField(
                    structDecl = structDecl,
                    memberIndex = index,
                    type = reduceGenericInstances(fieldType)
                )
            } else {
                null
            }
        }
    }
    private fun resolveStructFieldBinding(expression: Expression.Property): PropertyBinding? {
        // TODO: Remove this inferExpression call.
        //       This function can be called to "try" if the property expression is a struct field binding.
        //       but in case when expression.lhs refers to something that's not a real expression, e.g. a module path
        //       access or a enum constructor call, inferExpression would fail.
        //       Solving this would probably require adding a "Module" type so that every expression can be treated as
        //       having a type, making the type checker much simpler to reason about.
        val lhsType = inferExpression(expression.lhs)
        return resolveStructFieldBinding(lhsType, expression.property)
    }

    private fun getStructDeclOfType(lhsType: Type): Declaration.Struct? {
        return when (lhsType) {
            is Type.Constructor -> {
                ctx.resolver.resolveDeclaration(lhsType.name) as? Declaration.Struct
            }
            is Type.Application -> {
                getStructDeclOfType(lhsType.callee)
            }
            else -> null
        }
    }

    private fun equateTypes(source: Type, destination: Type) {
        // isTypeAssignable is side effectful in the sense that it instantiates
        // Type.GenericInstance types for matching source and destination types
        isTypeAssignableTo(source, destination)
    }

    fun isTypeAssignableTo(source: Type, destination: Type): Boolean {
        return typeAnalyzer.isTypeAssignableTo(
            source = source,
            destination = destination,
        )
    }

    private val typeOfExpressionCache = MutableNodeMap<Expression, Type>()
    private fun typeOfExpression(expression: Expression): Type {
        val cached = typeOfExpressionCache[expression]
        if (cached != null) {
            return cached
        }
        val def = ctx.resolver.getEnclosingFunction(expression)
        if (def != null) {
            visitDeclaration(def)
        } else {
            val sourceFile = ctx.resolver.getSourceFile(expression.location.file)
            sourceFile.declarations.forEach { visitDeclaration(it) }
        }

        return requireNotNull(typeOfExpressionCache[expression]) {
            "${expression.location}"
        }
    }

    private val visitedDeclarationSet = MutableNodeMap<Declaration, Unit>()
    private fun visitDeclaration(declaration: Declaration) = visitedDeclarationSet.getOrPut(declaration) {
        exhaustive(
            when (declaration) {
                is Declaration.FunctionDef -> visitFunctionDef(declaration)
                is Declaration.ConstDefinition -> visitConstDef(declaration)
                is Declaration.Enum -> visitEnumDef(declaration)
                is Declaration.Error -> Unit
                is Declaration.ExtensionDef -> visitExtensionDef(declaration)
                is Declaration.ExternConst -> visitExternConstDef(declaration)
                is Declaration.ExternFunctionDef -> visitExternFunctionDef(declaration)
                is Declaration.ImportAs -> Unit
                is Declaration.ImportMembers -> Unit
                is Declaration.Struct -> visitStructDef(declaration)
                is Declaration.TypeAlias -> visitTypeAlias(declaration)
            }
        )
    }

    private fun visitTypeAlias(declaration: Declaration.TypeAlias) {
        annotationToType(declaration.rhs)
    }

    private fun visitStructDef(declaration: Declaration.Struct) {
        for (member in declaration.members) {
            when (member) {
                is Declaration.Struct.Member.Field -> {
                    annotationToType(member.typeAnnotation)
                }
            }
        }
    }

    private fun visitExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        for (paramType in declaration.paramTypes) {
             annotationToType(paramType)
        }
        annotationToType(declaration.returnType)
    }

    private fun visitEnumDef(declaration: Declaration.Enum) {
        for (case in declaration.cases) {
            case.params?.forEach {
                annotationToType(it.annotation)
            }
        }
    }

    private fun visitExternConstDef(declaration: Declaration.ExternConst) {
        annotationToType(declaration.type)
    }

    private fun visitExtensionDef(declaration: Declaration.ExtensionDef) {
        annotationToType(declaration.forType)
        declaration.functionDefs.forEach { visitFunctionDef(it) }
    }

    private fun visitConstDef(declaration: Declaration.ConstDefinition) {
        val annotatedType = declaration.annotation?.let { annotationToType(it) }

        if (annotatedType != null) {
            checkExpression(declaration.initializer, annotatedType)
        } else {
            inferExpression(declaration.initializer)
        }
    }

    private fun visitFunctionDef(def: Declaration.FunctionDef) {
        for (param in def.params) {
            param.annotation?.let { annotationToType(it) }
        }
        val returnType = annotationToType(def.signature.returnType)
        returnTypeStack.push(returnType)
        val oldEnv = env
        env = Env.ofFunction(
            def,
            parent = env,
            lowerType = { annotationToType(it) }
        )
        checkBlock(def.body)
        check(oldEnv === env.parent)
        env = requireNotNull(env.parent)
        returnTypeStack.pop()
    }

    private fun checkBlock(block: Block) {
        val oldEnv = env
        for (member in block.members) {
            checkBlockMember(member)
        }
        env = oldEnv
    }

    private fun checkBlockMember(member: Block.Member): Unit = when (member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> checkStatement(member.statement)
    }

    private fun checkStatement(statement: Statement): Unit = when (statement) {
        is Statement.Return -> visitReturnStatement(statement)
        is Statement.Val -> visitValStatement(statement)
        is Statement.While -> visitWhileStatement(statement)
        is Statement.If -> visitIfStatement(statement)
        is Statement.LocalAssignment -> visitLocalAssignment(statement)
        is Statement.MemberAssignment -> visitMemberAssignment(statement)
        is Statement.PointerAssignment -> visitPointerAssignment(statement)
        is Statement.Defer -> visitDeferStatement(statement)
        is Statement.Error -> Unit
    }

    private fun visitDeferStatement(statement: Statement.Defer) {
        checkBlockMember(statement.blockMember)
    }

    private fun visitPointerAssignment(statement: Statement.PointerAssignment) {
        val valueType = inferExpression(statement.lhs)
        val ptrType = inferExpression(statement.lhs.expression)

        if (ptrType !is Type.Ptr) {
            inferExpression(statement.value)
            return
        }
        checkExpression(statement.value, valueType)
    }

    private fun visitLocalAssignment(statement: Statement.LocalAssignment) {
        val lhsType = inferIdentifier(statement.name)
        checkExpression(statement.value, lhsType)
    }

    private fun visitMemberAssignment(statement: Statement.MemberAssignment) {
        val lhsType = inferExpression(statement.lhs)
        checkExpression(statement.value, lhsType)
        val field = resolvePropertyBinding(statement.lhs)
        if (field !is PropertyBinding.StructField) {
            return
        }

        if (statement.lhs.lhs !is Expression.Var) {
            return
        }
        val binding = ctx.resolver.resolve(statement.lhs.lhs.name) ?: return
        if (binding !is Binding.ValBinding) {
            return
        }
    }

    private fun checkAssignability(source: Type, destination: Type) {
        isTypeAssignableTo(destination = destination, source = source)
    }

    private fun visitWhileStatement(statement: Statement.While) {
        checkExpression(statement.condition, Type.Bool)
        checkBlock(statement.body)
    }

    private fun visitIfStatement(statement: Statement.If) {
        checkExpression(statement.condition, Type.Bool)
        checkBlock(statement.ifTrue)
        statement.ifFalse?.let { checkBlock(it) }
    }

    private fun visitReturnStatement(statement: Statement.Return) {
        if (statement.value != null) {
            val returnType = returnTypeStack.peek()
            if (returnType != null) {
                checkExpression(statement.value, returnType)
            } else {
                inferExpression(statement.value)
            }
        }
    }

    private fun visitValStatement(statement: Statement.Val) {
        val expectedType = statement.typeAnnotation?.let { annotationToType(it) }
        val type = if (expectedType != null) {
            checkExpression(statement.rhs, expectedType)
            expectedType
        } else {
            inferExpression(statement.rhs)
        }
        env = Env.Let(
            parent = env,
            name = statement.binder.name,
            type = type,
        )
    }

    private fun isPredicateOperator(operator: BinaryOperator): Boolean {
        return (
            isEqualityCheckingOperator(operator) ||
                isOrderPredicateOperator(operator)
            )
    }

    private fun isEqualityCheckingOperator(operator: BinaryOperator): Boolean {
        return (
            operator == BinaryOperator.EQUALS ||
                operator == BinaryOperator.NOT_EQUALS
            )
    }

    private fun isOrderPredicateOperator(operator: BinaryOperator): Boolean {
        return (
            operator == BinaryOperator.GREATER_THAN ||
                operator == BinaryOperator.GREATER_THAN_EQUAL ||
                operator == BinaryOperator.LESS_THAN ||
                operator == BinaryOperator.LESS_THAN_EQUAL
            )
    }

    private fun checkExpression(expression: Expression, expectedType: Type): Type {
        val type = if (expression is Expression.BinaryOperation) {
            if (isPredicateOperator(expression.operator)) {
                val lhsType = inferExpression(expression.lhs)
                checkExpression(expression.rhs, lhsType)
                Type.Bool
            } else {
                checkExpression(expression.lhs, expectedType)
                checkExpression(expression.rhs, expectedType)
            }
        } else {
            when (expression) {
                is Expression.IntLiteral -> {
                    val type = if (isIntLiteralAssignable(expectedType)) {
                        expectedType
                    } else {
                        inferExpression(expression)
                    }
                    checkAssignability(source = type, destination = expectedType)
                    type
                }
                is Expression.Uninitialized -> expectedType
                is Expression.FloatLiteral -> {
                    if (expectedType is Type.FloatingPoint) {
                        expectedType
                    } else {
                        Type.Error(expression.location)
                    }
                }
                is Expression.Call -> {
                    checkCallExpression(expression, expectedType)
                }
                is Expression.NullPtr -> checkNullPtrExpression(expression, expectedType)
                is Expression.Closure -> checkOrInferClosureExpression(expression, expectedType)
                is Expression.If -> checkIfExpression(expression, expectedType)
                is Expression.UnaryMinus -> checkUnaryMinus(expression, expectedType)
                is Expression.Match -> checkMatchExpression(expression, expectedType)
                else -> {
                    val inferredType = inferExpression(expression)
                    checkAssignability(source = inferredType, destination = expectedType)
                    inferredType
                }
            }
        }
        typeOfExpressionCache[expression] = type
        return type
    }

    private fun checkUnaryMinus(expression: Expression.UnaryMinus, expectedType: Type): Type {
        checkExpression(expression.expression, expectedType)
        return expectedType
    }

    private fun checkIfExpression(expression: Expression.If, expectedType: Type): Type {
        checkExpression(expression.condition, Type.Bool)
        checkExpression(expression.trueBranch, expectedType)
        checkExpression(expression.falseBranch, expectedType)
        return expectedType
    }

    private val closureParamTypes = MutableNodeMap<Binder, Type>()
    private val inferredParamTypes = MutableNodeMap<Binder, Type>()
    private fun checkOrInferClosureExpression(expression: Expression.Closure, expectedType: Type?): Type {
        val functionTypeComponents = expectedType?.let { getFunctionTypeComponents(it) }
        val expectedReturnType = functionTypeComponents?.to ?: expression.returnType?.let { annotationToType(it) }
        if (functionTypeComponents != null && expression.returnType != null) {
            checkAssignability(
                source = annotationToType(expression.returnType),
                destination = functionTypeComponents.to
            )
        }
        returnTypeStack.push(expectedReturnType)
        val expectedParamTypes = expectedType?.let { getFunctionTypeComponents(it) }
        var index = -1
        val paramTypes = mutableListOf<Type>()
        for (param in expression.params) {
            index++
            val expectedParamType = expectedParamTypes?.from?.getOrNull(index)
            val type = when {
                param.annotation != null -> {
                    val annotatedType = annotationToType(param.annotation)
                    if (expectedParamType != null) {
                        checkAssignability(
                            // params are contravariant
                            destination = annotatedType,
                            source = expectedParamType
                        )
                    }
                    annotatedType
                }
                expectedParamType != null -> {
                    inferredParamTypes[param.binder] = expectedParamType
                    expectedParamType
                }
                else -> {
                    Type.Error(param.location)
                }
            }
            closureParamTypes[param.binder] = type
            paramTypes.add(type)
        }

        val returnType = when (expression.body) {
            is ClosureBody.Block -> {
                checkBlock(expression.body.block)
                expectedReturnType ?: Type.Error(expression.body.location)
            }
            is ClosureBody.Expression -> {
                if (expectedReturnType != null) {
                    checkExpression(expression.body.expression, expectedReturnType)
                } else {
                    inferExpression(expression.body.expression)
                }
            }
        }
        return Type.Closure(
            from = paramTypes,
            to = returnType
        )
    }

    fun getInferredParamType(param: Param): Type? {
        val function = ctx.resolver.getEnclosingFunction(param)
        if (function == null) {
            ctx.resolver.getSourceFile(param.binder.location.file).let { sourceFile ->
                sourceFile.declarations.forEach {
                    visitDeclaration(it)
                }
            }
        } else {
            visitDeclaration(function)
        }
        return inferredParamTypes[param.binder]
    }

    private fun checkNullPtrExpression(expression: Expression.NullPtr, expectedType: Type): Type = when (expectedType) {
        !is Type.Ptr -> Type.Ptr(Type.Error(expression.location), isMutable = false)
        else -> expectedType
    }

    private fun isIntLiteralAssignable(type: Type): Boolean = when (reduceGenericInstances(type)) {
        is Type.Size,
        is Type.Integral -> true
        is Type.FloatingPoint -> true
        else -> false
    }

    private fun inferExpression(expression: Expression): Type = typeOfExpressionCache.getOrPut(expression) {
        reduceGenericInstances(
            when (expression) {
                is Expression.Error -> Type.Error(expression.location)
                is Expression.Var -> inferVarExpresion(expression)
                is Expression.Call -> inferCallExpression(expression)
                is Expression.Property -> inferPropertyExpression(expression)
                is Expression.CString -> Type.CChar.ptr()
                is Expression.BoolLiteral -> Type.Bool
                is Expression.NullPtr -> inferNullPtrExpression(expression)
                is Expression.IntLiteral -> inferIntLiteral(expression)
                is Expression.Not -> inferNotExpression(expression)
                is Expression.BinaryOperation -> inferBinaryOperation(expression)
                is Expression.SizeOf -> inferSizeOfExpression(expression)
                is Expression.AlignOf -> inferAlignOfExpression(expression)
                is Expression.AddressOf -> inferAddressOf(expression)
                is Expression.AddressOfMut -> inferAddressOfMut(expression)
                is Expression.Deref -> inferDerefExpression(expression)
                is Expression.PointerCast -> inferPointerCast(expression)
                is Expression.If -> inferIfExpression(expression)
                is Expression.TypeApplication -> inferTypeApplication(expression)
                is Expression.This -> inferThisExpression(expression)
                is Expression.Closure -> checkOrInferClosureExpression(expression, expectedType = null)
                is Expression.As -> {
                    inferExpression(expression.lhs)
                    annotationToType(expression.rhs)
                }
                is Expression.BlockExpression -> inferBlockExpression(expression)
                is Expression.Intrinsic -> inferIntrinsicExpression(expression)
                is Expression.UnaryMinus -> inferUnaryMinus(expression)
                is Expression.ByteCharLiteral -> Type.Integral(8, isSigned = false)
                is Expression.Match -> inferMatchExpression(expression)
                is Expression.FloatLiteral -> Type.f64
                is Expression.Uninitialized -> Type.Error(expression.location) // uninitialized expression must be checked, not inferred
                is Expression.Move -> inferIdentifier(expression.name)
            }
        )
    }

    private fun inferMatchExpression(expression: Expression.Match): Type {
        val discriminantType = inferExpression(expression.value)
        var expectedType: Type? = null

        expression.arms.forEachIndexed { index, arm ->
            val oldEnv = env
            env = extendEnvWithPattern(env, arm.pattern, discriminantType)
            if (index == 0) {
                expectedType = inferExpression(arm.value)
            } else {
                checkExpression(arm.value, checkNotNull(expectedType))
            }
            env = oldEnv

        }
        return expectedType ?: Type.Error(expression.location, "Match expression with no arms")
    }

    private fun inferUnaryMinus(expression: Expression.UnaryMinus): Type {
        return inferExpression(expression.expression)
    }

    private fun inferIntrinsicExpression(expression: Expression.Intrinsic): Type =
        when (expression.intrinsicType) {
            IntrinsicType.ADD, IntrinsicType.SUB, IntrinsicType.MUL -> {
                val typeParam = Binder(
                    Identifier(expression.location, name = ctx.makeName("T")),
                    ctx.makeBinderId(),
                )
                Type.ForAll(
                    listOf(Type.Param(typeParam)),
                    Type.FunctionPtr(
                        from = listOf(
                            Type.Param(typeParam),
                            Type.Param(typeParam)
                        ),
                        to = Type.Param(typeParam),
                    )
                )
            }
            IntrinsicType.PTR_TO_INT -> {
                val typeParam = Binder(
                    Identifier(expression.location, name = ctx.makeName("T")),
                    ctx.makeBinderId())
                Type.ForAll(
                    listOf(Type.Param(typeParam)),
                    Type.FunctionPtr(
                        from = listOf(
                            Type.Ptr(Type.Param(typeParam), isMutable = false)
                        ),
                        to = Type.Size(isSigned = false),
                    )
                )
            }
            IntrinsicType.INT_TO_PTR -> {
                val typeParam = Binder(
                    Identifier(expression.location, name = ctx.makeName("T")),
                    ctx.makeBinderId(),
                )
                Type.ForAll(
                    listOf(Type.Param(typeParam)),
                    Type.FunctionPtr(
                        from = listOf(
                            Type.Size(isSigned = false)
                        ),
                        to = Type.Param(typeParam),
                    )
                )
            }
            IntrinsicType.MEMCPY -> {
                val l1 = expression.location
                val t1 = Type.Param(Binder(Identifier(l1, ctx.makeName("T1")), ctx.makeBinderId()))

                Type.ForAll(
                    listOf(t1),
                    Type.FunctionPtr(
                        from = listOf(
                            Type.Param(t1.binder).mutPtr(), // destination
                            Type.Param(t1.binder).ptr(), // source
                            Type.usize // number of items to copy
                        ),
                        to = Type.Void
                    )
                )
            }
            IntrinsicType.ERROR -> Type.Error(expression.location)
        }

    private fun inferBlockExpression(expression: Expression.BlockExpression): Type {
        val oldEnv = env
        val lastExpression = expression.block.members.lastOrNull() ?: return Type.Void

        expression.block.members.dropLast(1).forEach {
            checkBlockMember(it)
        }

        when (lastExpression) {
            is Block.Member.Expression -> {
                return inferExpression(lastExpression.expression)
            }
            is Block.Member.Statement -> {
                checkStatement(lastExpression.statement)
            }
        }
        env = oldEnv
        return Type.Void
    }

    private fun checkMatchExpression(expression: Expression.Match, expectedType: Type): Type {
        val discriminantType = inferExpression(expression.value)
        for (arm in expression.arms) {
            val oldEnv = env
            env = extendEnvWithPattern(env, arm.pattern, discriminantType)
            checkExpression(arm.value, expectedType)
            env = oldEnv
        }
        return expectedType
    }

    private fun extendEnvWithPattern(prevEnv: Env, pattern: Pattern, discriminantType: Type): Env {
        when (pattern) {
            is Pattern.Wildcard,
            is Pattern.IntLiteral -> Unit
            is Pattern.Val -> {
                return Env.Let(
                    parent = prevEnv,
                    name = pattern.binder.name,
                    type = discriminantType,
                )
            }
            is Pattern.EnumCase -> {
                val decl = getEnumTypeDeclaration(discriminantType)
                val case = decl?.getCase(pattern.identifier.name) ?: return env
                var resultEnv = env
                for ((param, arg) in case.first.params?.zip(pattern.args ?: emptyList()) ?: emptyList()) {
                    val paramType = annotationToType(param.annotation).applyTypeParams(
                        decl.typeParams,
                        discriminantType.typeArgs(),
                    )
                    resultEnv = extendEnvWithPattern(
                        resultEnv,
                        arg,
                        paramType
                    )
                }
                return resultEnv
            }
        }
        return env
    }

    private fun inferTypeApplication(expression: Expression.TypeApplication): Type {
        val lhsType = inferExpression(expression.lhs)
        return if (lhsType is Type.ForAll) {
            val substitution = lhsType.params.zip(expression.args).associate { (param, annotation) ->
                param.binder.id to annotationToType(annotation)
            }.toSubstitution()
            getTypeArgsCache[expression.lhs] = expression.args.map { annotationToType(it) }
            lhsType.body.applySubstitution(substitution)
        } else {
            Type.Error(expression.location)
        }
    }

    private fun inferThisExpression(expression: Expression.This): Type {
        val extensionDef = ctx.resolver.getEnclosingExtensionDef(expression)
        val enclosingFunction = ctx.resolver.getEnclosingFunction(expression)
        if (extensionDef == null || enclosingFunction == null || enclosingFunction.signature.thisParamFlags == null) {
            return Type.Error(expression.location)
        }
        val thisType = annotationToType(extensionDef.forType)
        val thisParamFlags = enclosingFunction.signature.thisParamFlags
        return if (thisParamFlags.isPointer) {
            Type.Ptr(thisType, thisParamFlags.isMutable)
        } else {
            thisType
        }
    }

    private fun inferDerefExpression(expression: Expression.Deref): Type {
        val ptrType = inferExpression(expression.expression)
        return if (ptrType !is Type.Ptr) {
            Type.Error(expression.location)
        } else {
            ptrType.to
        }
    }

    private fun inferSizeOfExpression(expression: Expression.SizeOf): Type {
        annotationToType(expression.type)
        return Type.Size(isSigned = false)
    }
    private fun inferAlignOfExpression(expression: Expression.AlignOf): Type {
        annotationToType(expression.type)
        return Type.usize
    }

    private fun inferBinaryOperation(expression: Expression.BinaryOperation): Type {
        val lhsType = inferExpression(expression.lhs)
        val matchingRule = BIN_OP_RULES[expression.operator to lhsType]
        return if (matchingRule != null) {
            checkExpression(expression.rhs, matchingRule.first)
            matchingRule.second
        } else {
            if ((expression.operator == BinaryOperator.EQUALS || expression.operator == BinaryOperator.NOT_EQUALS) &&
                isTypeEquatable(lhsType)
            ) {
                checkExpression(expression.rhs, if (lhsType is Type.Ptr) lhsType.copy(isMutable = false) else lhsType)
                Type.Bool
            } else if ((lhsType.isIntegral() || lhsType is Type.FloatingPoint) && expression.operator in ARITHMETIC_OPERATORS) {
                checkExpression(expression.rhs, lhsType)
                lhsType
            } else {
                inferExpression(expression.rhs)
                Type.Error(expression.location)
            }
        }
    }

    private fun isTypeEquatable(lhsType: Type): Boolean = when (reduceGenericInstances(lhsType)) {
        is Type.Ptr,
        is Type.Bool,
        is Type.Integral,
        is Type.Size -> true
        else -> false
    }

    private fun inferPointerCast(expression: Expression.PointerCast): Type {
        val targetPointerToType = annotationToType(expression.toType)
        val argType = inferExpression(expression.arg)
        return if (argType !is Type.Ptr) {
            Type.Ptr(targetPointerToType, isMutable = true)
        } else {
            Type.Ptr(targetPointerToType, isMutable = argType.isMutable)
        }
    }

    private fun inferNullPtrExpression(expression: Expression.NullPtr): Type {
        return Type.Error(expression.location)
    }

    private fun inferAddressOfMut(expression: Expression.AddressOfMut): Type {
        return Type.Ptr(inferExpression(expression.expression), isMutable = true)
    }

    private fun inferAddressOf(expression: Expression.AddressOf): Type {
        return Type.Ptr(inferExpression(expression.expression), isMutable = false)
    }

    private fun inferIntLiteral(
        @Suppress("UNUSED_PARAMETER")
        expression: Expression.IntLiteral
    ): Type {
        return Type.Integral(32, true)
    }

    private fun inferIfExpression(expression: Expression.If): Type {
        checkExpression(expression.condition, Type.Bool)
        val type = inferExpression(expression.trueBranch)
        checkExpression(expression.falseBranch, type)
        return type
    }

    private fun inferNotExpression(expression: Expression.Not): Type {
        return checkExpression(expression.expression, Type.Bool)
    }

    private fun inferPropertyExpression(expression: Expression.Property): Type =
        when (val binding = resolvePropertyBinding(expression)) {
            is PropertyBinding.Global -> typeOfGlobalPropertyBinding(binding)
            is PropertyBinding.StructField -> binding.type
            is PropertyBinding.StructPointerFieldLoad -> binding.type
            is PropertyBinding.ExtensionDef -> binding.type
            is PropertyBinding.EnumTypeCaseConstructor -> binding.type
            null -> Type.Error(expression.location)
        }

    private fun typeOfGlobalPropertyBinding(
        binding: PropertyBinding.Global
    ): Type {
        return typeOfBinding(binding.binding)
    }

    private fun inferVarExpresion(expression: Expression.Var): Type {
        return inferIdentifier(expression.name)
    }

    private fun inferIdentifier(name: Identifier): Type {
        val fromEnv = env.resolveValue(name.name)
        if (fromEnv != null) {
            return fromEnv
        }
        return when (val binding = ctx.resolver.resolve(name)) {
            // TODO: Remove `typeOfBinding`
            is Binding.ValBinding,
            is Binding.GlobalConst,
            is Binding.ExternConst,
            is Binding.ExternFunction,
            is Binding.GlobalFunction,
            is Binding.Struct,
            is Binding.MatchArmEnumCaseArg,
            is Binding.FunctionParam,
                -> requireUnreachable {
                    "The type of ${binding::class.simpleName} should be handled by the env"
                }
            is Binding.ClosureParam,
            is Binding.Enum,
                -> typeOfBinding(binding)
            null -> {
                Type.Error(name.location, "Unbound variable: ${name.name.text}")
            }
        }
    }

    private fun typeOfBinding(binding: Binding): Type = when (binding) {
        is Binding.GlobalFunction -> typeOfGlobalFunctionRef(binding.declaration)
        is Binding.ExternFunction -> typeOfExternFunctionRef(binding.declaration)
        is Binding.FunctionParam -> typeOfParam(binding.declaration, binding.index)
        is Binding.ValBinding -> typeOfValRef(binding)
        is Binding.Struct -> typeOfStructValueRef(binding)
        is Binding.GlobalConst -> typeOfGlobalConstBinding(binding)
        is Binding.ClosureParam -> typeOfClosureParam(binding)
        is Binding.Enum -> Type.Error(binding.declaration.location)
        is Binding.ExternConst -> typeOfExternConstBinding(binding)
        is Binding.MatchArmEnumCaseArg -> typeOfMatchArmEnumCaseArgBinding(binding)
    }

    fun typeOfMatchArmEnumCaseArgBinding(binding: Binding.MatchArmEnumCaseArg): Type {
        val discriminantPattern = binding.topLevelPattern
        val matchExpression = checkNotNull(ctx.resolver.getEnclosingMatchExpression(binding.arg))
        val discriminantType = typeOfExpression(matchExpression.value)
        val enumDecl = getEnumTypeDeclaration(discriminantType) ?: return Type.Error(binding.binder.location)
        val (case, _) = enumDecl.getCase(discriminantPattern.identifier.name) ?: return Type.Error(binding.binder.location)
        val declaredType = annotationToType(checkNotNull(case.params)[binding.argIndex].annotation)

        if (enumDecl.typeParams == null) {
            return declaredType
        }

        val subst: Substitution = enumDecl.typeParams.zip(discriminantType.typeArgs()).associate { (param, arg) ->
            param.binder.id to arg
        }.toSubstitution()
        return declaredType.applySubstitution(subst)
    }

    private fun typeOfExternConstBinding(binding: Binding.ExternConst): Type {
        return annotationToType(binding.declaration.type)
    }

    private fun typeOfClosureParam(binding: Binding.ClosureParam): Type {
        return closureParamTypes[binding.param.binder] ?: Type.Error(binding.closure.location)
    }

    private fun typeOfGlobalConstBinding(binding: Binding.GlobalConst): Type {
        return if (binding.declaration.annotation != null) {
            annotationToType(binding.declaration.annotation)
        } else {
            inferExpression(binding.declaration.initializer)
        }
    }

    private fun typeOfStructValueRef(binding: Binding.Struct): Type {
        val name = ctx.resolver.resolveGlobalName(binding.declaration.binder)
        val constrType = Type.Constructor(name)
        val instanceType = if (binding.declaration.typeParams == null) {
            constrType
        } else {
            Type.Application(
                constrType,
                binding.declaration.typeParams.map { Type.Param(it.binder) }
            )
        }
        val fieldTypes = structFieldTypes(binding.declaration).values.toList()
        val functionPtrType =
            Type.FunctionPtr(from = fieldTypes, to = instanceType)
        return if (binding.declaration.typeParams == null) {
            functionPtrType
        } else {
            Type.ForAll(
                params = binding.declaration.makeTypeParams(),
                body = functionPtrType
            )
        }
    }

    private fun structFieldTypes(declaration: Declaration.Struct): Map<Name, Type> {
        return declaration.members.associate {
            when (it) {
                is Declaration.Struct.Member.Field ->
                    it.binder.identifier.name to annotationToType(it.typeAnnotation)
            }
        }
    }

    private fun typeOfValRef(binding: Binding.ValBinding): Type {
        return if (binding.statement.typeAnnotation != null) {
            annotationToType(binding.statement.typeAnnotation)
        } else {
            inferExpression(binding.statement.rhs)
        }
    }

    private fun typeOfParam(declaration: Declaration.FunctionDef, paramIndex: Int): Type {
        val annotation = declaration.params[paramIndex].annotation
        return if (annotation == null) {
            Type.Error(declaration.location)
        } else {
            annotationToType(annotation)
        }
    }

    private fun typeOfExternFunctionRef(declaration: Declaration.ExternFunctionDef): Type {
        return Type.FunctionPtr(
            from = declaration.paramTypes.map { annotationToType(it) },
            to = annotationToType(declaration.returnType),
        )
    }

    private fun typeOfGlobalFunctionRef(declaration: Declaration.FunctionDef): Type {
        val functionPtrType = Type.FunctionPtr(
            from = declaration.params.map { param ->
                param.annotation?.let { annotationToType(it) } ?: Type.Error(param.location)
            },
            to = annotationToType(declaration.signature.returnType),
        )
        val typeParams = declaration.typeParams
        return if (typeParams != null) {
            Type.ForAll(
                params = declaration.makeTypeParams(),
                body = functionPtrType
            )
        } else {
            functionPtrType
        }
    }

    data class FunctionTypeComponents(
        val from: List<Type>,
        val to: Type,
        val typeParams: List<Type.Param>?,
    )

    private fun inferCallExpression(expression: Expression.Call): Type {
        return inferOrCheckCallLikeExpression(
            callNode = expression,
            args = expression.args,
            expectedReturnType = null
        )
    }
    private fun checkCallExpression(expression: Expression.Call, expectedType: Type): Type {
        return inferOrCheckCallLikeExpression(
            callNode = expression,
            args = expression.args,
            expectedReturnType = expectedType
        )
    }

    private fun inferOrCheckCallLikeExpression(
        callNode: Expression.Call,
        args: List<Arg>,
        expectedReturnType: Type?
    ): Type {
        val calleeType = getCalleeType(callNode)
        val callee = getCallee(callNode)
        val functionType = getFunctionTypeComponents(calleeType)
        return if (functionType == null) {
            for (arg in args) {
                inferExpression(arg.expression)
            }
            Type.Error(callNode.location)
        } else {
            check(callee != null)
            val substitution = instantiateSubstitution(
                functionType.typeParams
            )

            if (functionType.typeParams != null) {
                val genericCallee = when (callee) {
                    is Expression.TypeApplication -> callee.lhs
                    else -> callee
                }
                getTypeArgsCache[genericCallee] = functionType.typeParams.map {
                    requireNotNull(substitution[it.binder.id])
                }
            }
            val receiver = getCallReceiver(callNode)
            val argsWithReceiver = if (receiver == null) {
                args.map { it.expression }
            } else {
                listOf(receiver) + args.map { it.expression }
            }
            val explicitTypeArgs = when (callee) {
                is Expression.TypeApplication -> callee.args.map { annotationToType(it) }
                else -> null
            }
            checkCallArgs(functionType, explicitTypeArgs, argsWithReceiver, substitution)
            if (expectedReturnType != null) {
                val source = functionType.to.applySubstitution(substitution)
                checkAssignability(
                    source = source,
                    destination = expectedReturnType
                )
            }

            reduceGenericInstances(functionType.to.applySubstitution(substitution))
        }
    }

    fun getCallReceiver(callNode: Expression.Call): Expression? = when (callNode.callee) {
        is Expression.Property -> getPropertyExpressionReceiver(callNode.callee)
        is Expression.TypeApplication -> when (callNode.callee.lhs) {
            is Expression.Property -> getPropertyExpressionReceiver(callNode.callee.lhs)
            else -> null
        }
        else -> null
    }

    private fun getPropertyExpressionReceiver(expression: Expression.Property): Expression? =
        when (resolvePropertyBinding(expression)) {
            is PropertyBinding.ExtensionDef -> {
                expression.lhs
            }
            else -> {
                null
            }
        }

    fun getCalleeType(callNode: Expression): Type = getCallee(callNode)?.let {
        inferExpression(it)
    } ?: Type.Error(callNode.location)

    private fun getCallee(callNode: Expression): Expression? = when (callNode) {
        is Expression.Call -> callNode.callee
        else -> null
    }

    /**
     * Takes type params and creates a fresh substitution mapping each type param
     * with a fresh type hole (Type.GenericInstance) that is assignable to any type.
     * If we try to check a type against this new type hole, if the hole is unassigned,
     * we assume that the types match and assign the type to the specific hole.
     * For example, when doing something like
     * // assume id : [T](T) -> T
     * id(true)
     * We instantiate the `T` param to Type.GenericInstance(1)
     * Then when checking the call args, when we try to check
     * isTypeAssignable( destination = Type.GenericInstance(1), source = Bool ),
     * we see that Type.GenericInstance(1) isn't instantiated, so it type checks fine.
     *
     */
    private fun instantiateSubstitution(
        typeParams: List<Type.Param>?
    ): Substitution {
        return typeParams?.associate {
            it.binder.id to makeGenericInstance(
                it.binder
            )
        }?.toSubstitution() ?: emptySubstitution()
    }

    private fun checkCallArgs(
        functionType: FunctionTypeComponents,
        typeArgs: List<Type>?,
        args: List<Expression>,
        substitution: Substitution
    ) {
        val length = min(functionType.from.size, args.size)
        typeArgs?.zip(functionType.typeParams ?: emptyList())?.forEach { (arg, param) ->
            val expectedType = Type.Param(param.binder).applySubstitution(substitution)
            equateTypes(source = arg, destination = expectedType)
        }

        for (i in 0 until length) {
            val expectedType = functionType.from[i].applySubstitution(substitution)
            val arg = args[i]
            checkExpression(arg, expectedType)
        }
        for (arg in args.drop(length)) {
            inferExpression(arg)
        }
    }

    fun getFunctionTypeComponents(type: Type): FunctionTypeComponents? {
        fun recurse(type: Type, typeParams: List<Type.Param>?): FunctionTypeComponents? {
            return when (type) {
                is Type.FunctionPtr ->
                    FunctionTypeComponents(
                        from = type.from,
                        to = type.to,
                        typeParams = typeParams,
                    )
                is Type.Closure ->
                    FunctionTypeComponents(
                        from = type.from,
                        to = type.to,
                        typeParams = null,
                    )
                is Type.ForAll -> {
                    recurse(type.body, type.params)
                }
                is Type.GenericInstance -> {
                    if (typeAnalyzer.getInstantiatedType(type) == null) {
                        null
                    } else {
                        recurse(reduceGenericInstances(type), null)
                    }
                }
                is Type.Error -> {
                    return null
                }
                else -> null
            }
        }
        return recurse(type, null)
    }

    private fun makeGenericInstance(
        binder: Binder
    ): Type.GenericInstance {
        return typeAnalyzer.makeGenericInstance(binder)
    }

    private val getTypeArgsCache = MutableNodeMap<Expression, List<Type>>()
    fun getTypeArgs(expression: Expression): List<Type>? {
        typeOfExpression(expression) // ensure that the surrounding context has been typed
        return getTypeArgsCache[expression]?.map { reduceGenericInstances(it) }
    }

    private fun annotationToType(annotation: TypeAnnotation): Type =
        astConv.typeAnnotationToType(annotation)

    fun typeOfBinder(binder: Binder): Type = when (val binding = ctx.resolver.resolve(binder.identifier)) {
        null -> Type.Error(binder.location)
        else -> typeOfBinding(binding)
    }

    fun reduceGenericInstances(type: Type): Type {
        return object : TypeTransformer {
            override fun lowerGenericInstance(type: Type.GenericInstance): Type {
                val existing = typeAnalyzer.getInstantiatedType(type)
                return if (existing != null) {
                    lowerType(existing)
                } else {
                    type
                }
            }

            override fun lowerTypeApplication(type: Type.Application): Type {
                return if (type.callee is Type.ForAll) {
                    require(type.callee.params.size == type.args.size)
                    val substitution = type.callee.params.zip(type.args).toSubstitution()
                    return type.callee.body.applySubstitution(substitution)
                } else {
                    super.lowerTypeApplication(type)
                }
            }
        }.lowerType(type)
    }

    fun getEnumTypeDeclaration(type: Type): Declaration.Enum? {
        val constructor = when (type) {
            is Type.Constructor -> type
            is Type.Application ->
                if (type.callee is Type.Constructor) {
                    type.callee
                } else {
                    return null
                }
            else -> return null
        }
        val declaration = ctx.resolver.resolveDeclaration(constructor.name)
        if (declaration is Declaration.Enum) {
            return declaration
        }
        return null
    }

    fun getEnumDiscriminants(declaration: Declaration.Enum, args: List<Type>): List<Discriminant> {
        val substitution = (declaration.typeParams ?: emptyList()).zip(args).toSubstitution()

        return declaration.cases.mapIndexed { index, case ->
            Discriminant(
                index,
                case.name.identifier.name,
                case.params?.map {
                    it.binder?.identifier?.name to
                        annotationToType(requireNotNull(it.annotation)).applySubstitution(substitution)
                } ?: singletonList(
                    ctx.makeName("dummy") to
                        Type.Integral(8, false)
                )
            )
        }
    }

    fun getEnumPayloadType(declaration: Declaration.Enum): Type.UntaggedUnion {
        val enumName = ctx.resolver.qualifiedName(declaration.name)
        return Type.UntaggedUnion(
            declaration.cases.map { case ->
                val constructorType = Type.Constructor(
                    enumName.append(case.name.identifier.name)
                )
                if (declaration.typeParams != null) {
                    Type.Application(constructorType, declaration.typeParams.map { Type.Param(it.binder) })
                } else {
                    constructorType
                }
            }
        )
    }

    fun isValidPropertyAccess(expression: Expression.Property): Boolean {
        typeOfExpression(expression)
        return resolvePropertyBinding(expression) != null
    }

    fun isCompileTimeConstant(initializer: Expression): Boolean {
        return when (initializer) {
            is Expression.IntLiteral,
            is Expression.BoolLiteral,
            is Expression.CString,
            is Expression.FloatLiteral,
            is Expression.Var -> true
            is Expression.Property -> {
                val binding = resolvePropertyBinding(initializer)
                binding is PropertyBinding.Global
            }
            is Expression.Call -> {
                val callee = initializer.callee
                isCompileTimeConstant(callee) && initializer.args.all { isCompileTimeConstant(it.expression) }
            }
            else -> {
                false
            }
        }
    }

    fun getEnumConstructorBinding(expression: Expression): PropertyBinding.EnumTypeCaseConstructor? =
        when (expression) {
            is Expression.Property -> {
                val binding = resolvePropertyBinding(expression)
                binding as? PropertyBinding.EnumTypeCaseConstructor
            }
            is Expression.TypeApplication -> getEnumConstructorBinding(expression.lhs)
            else -> null
        }

    fun getParamType(param: Param): Type {
        return reduceGenericInstances(requireNotNull(closureParamTypes[param.binder]))
    }

    fun getReturnType(expression: Expression): Type {
        return when (val exprType = typeOfExpression(expression)) {
            is Type.FunctionPtr -> reduceGenericInstances(exprType.to)
            is Type.Closure -> reduceGenericInstances(exprType.to)
            else -> Type.Error(expression.location)
        }
    }

    fun getClosureCaptures(closure: Expression.Closure): ClosureCaptures {
        val values = mutableMapOf<Binder, Pair<Binding.Local, Type>>()
        val types = mutableSetOf<Binder>()
        object : SyntaxVisitor {
            val typeVisitor = object : TypeVisitor {
                override fun visitParamRefType(type: Type.Param) {
                    if (!type.name.location.isWithin(closure.location)) {
                        types.add(type.name)
                    }
                }

                override fun visitTypeApplication(type: Type.Application) {
                    if (type.callee is Type.ForAll) {
                        val subst = type.callee.params.zip(type.args).toSubstitution()
                        visitType(
                            type.callee.body.applySubstitution(subst)
                        )
                    } else {
                        super.visitTypeApplication(type)
                    }
                }
            }

            override fun visitExpression(expression: Expression) {
                super.visitExpression(expression)
                if (!isTypedExpression(expression)) {
                    return
                }
                val type = reduceGenericInstances(typeOfExpression(expression))
                val typeArgs = getTypeArgs(expression)
                if (type is Type.ForAll && typeArgs != null) {
                    val subst = type.params.zip(typeArgs).toSubstitution()
                    typeVisitor.visitType(type.body.applySubstitution(subst))
                } else {
                    typeVisitor.visitType(type)
                }
            }

            override fun visitType(type: TypeAnnotation) {
                typeVisitor.visitType(annotationToType(type))
            }

            override fun visitVarExpr(expression: Expression.Var) {
                val binding = ctx.resolver.resolve(expression.name)
                if (binding is Binding.Local && !binding.isLocalTo(closure)) {
                    values[binding.binder] = binding to typeOfBinder(binding.binder)
                }
            }

            override fun visitLocalAssignment(statement: Statement.LocalAssignment) {
                val binding = ctx.resolver.resolve(statement.name)
                if (binding is Binding.Local && !binding.isLocalTo(closure)) {
                    values[binding.binder] = binding to typeOfBinder(binding.binder)
                }
                super.visitLocalAssignment(statement)
            }

            override fun visitVarType(type: TypeAnnotation.Var) {
                when (val binding = ctx.resolver.resolveTypeVariable(type.name)) {
                    is TypeBinding.TypeParam -> {
                        if (!binding.isLocalTo(closure)) {
                            types.add(binding.binder)
                        }
                    }
                    else -> unit
                }
            }
        }.visitExpression(closure)

        return ClosureCaptures(
            values,
            types
        )
    }

    /**
     * Returns true if the given expression has a type.
     * Not every syntactic expression has an actual type,
     * some of them need a property access to become a real expression
     * For example,
     *
     * enum E { X }
     * here, `E` is not a typed expression, even though it can occur at the LHS
     * of a property expression to become a typed expression (E.X)
     */
    private fun isTypedExpression(expression: Expression): Boolean {
        return when (expression) {
            is Expression.Var -> {
                when (ctx.resolver.resolve(expression.name)) {
                    is Binding.Enum -> false
                    null -> false
                    else -> true
                }
            }
            else -> {
                !isModuleRef(expression)
            }
        }
    }

    private fun isModuleRef(expression: Expression): Boolean =
        resolveModuleRef(expression) != null

    private fun resolveModuleRef(expression: Expression): SourceFile? = when (expression) {
        is Expression.Var -> {
            ctx.resolver.resolveModuleAlias(expression.name)
        }
        else -> null
    }

    fun isEnumType(type: Type): Boolean = getEnumTypeDeclaration(type) != null

    /**
     * Expression will be a capture if `expression`'s enclosing function/closure
     * is different from binding's enclosing function/closure.
     * Moreover, expression.enclosingClosureOrFunction should be a child scope
     * of binding.enclosingClosureOrFunction
     */

    fun isClosureCapture(expression: Identifier): Boolean {
        val binding = checkNotNull(ctx.resolver.resolve(expression))
        val expressionEnclosingScope = expression.enclosingClosureOrFunction()
        val bindingEnclosingScope = binding.binder.enclosingClosureOrFunction()

        check(expressionEnclosingScope.location.isWithin(bindingEnclosingScope.location))

        return expressionEnclosingScope !== bindingEnclosingScope
    }

    private fun HasLocation.enclosingClosureOrFunction(): ScopeTree {
        return ctx.resolver.getEnclosingClosure(this)
            ?: checkNotNull(ctx.resolver.getEnclosingFunction(this))
    }

    fun isRefStructType(type: Type): Boolean =
        getStructDeclOfType(type)?.isRef ?: false

    /**
     * Returns the struct declaration that this expression refers to,
     * null if it is not a reference to a struct def.
     */
    fun getStructDeclaration(expresssion: Expression): Declaration.Struct? = when (expresssion) {
        is Expression.Var ->
            ctx.resolver.resolve(expresssion.name)?.let {
                if (it is Binding.Struct) {
                    it.declaration
                } else {
                    null
                }
            }
        is Expression.Property -> {
            val moduleSourceFile = resolveModuleRef(expresssion.lhs)
            if (moduleSourceFile != null) {
                val binding = ctx.resolver.findInSourceFile(expresssion.property.name, moduleSourceFile)
                if (binding is Binding.Struct) binding.declaration else null
            } else {
                null
            }
        }
        else -> null
    }
}

data class ClosureCaptures(
    val values: Map<Binder, Pair<Binding.Local, Type>>,
    val types: Set<Binder>
)

class MutableNodeMap<T : HasLocation, V> {
    private val map = mutableMapOf<SourceLocation, V>()

    fun getOrPut(key: T, compute: () -> V): V {
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

    operator fun contains(key: T): Boolean {
        return key.location in map
    }
}

typealias op = BinaryOperator

val BIN_OP_RULES: Map<Pair<op, Type>, Pair<Type, Type>> = mapOf(

    (op.AND to Type.Bool) to (Type.Bool to Type.Bool),
    (op.OR to Type.Bool) to (Type.Bool to Type.Bool),

    (op.EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
    (op.NOT_EQUALS to Type.Bool) to (Type.Bool to Type.Bool)
)

data class Discriminant(
    val index: Int,
    val name: Name,
    val params: List<Pair<Name?, Type>>
)

interface PostAnalysisContext {
    val Expression.type: Type
    val Expression.Property.binding: PropertyBinding
    val Expression.Property.bindingOrNull: PropertyBinding?
    val TypeAnnotation.type: Type
}
