package hadesc.checker

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.exhaustive
import hadesc.ir.BinaryOperator
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
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
                    instanceType.callee
                }
            }
            is TypeBinding.Enum -> {
                val instanceType = typeOfEnumInstance(binding.declaration)
                if (binding.declaration.typeParams == null) {
                    instanceType
                } else {
                    require(instanceType is Type.Application)
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
                "Size" -> Type.Size
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
                require(callee is Type.Constructor)
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
            is TypeAnnotation.Function -> {
                Type.Function(
                        receiver = null,
                        typeParams = null,
                        from = annotation.from.map { inferAnnotation(it) },
                        to = inferAnnotation(annotation.to),
                        constraints = listOf()
                )
            }
            is TypeAnnotation.This -> resolveThisType(annotation)
        }
        annotationTypes[annotation] = type
        if (!allowIncomplete && type is Type.Constructor && type.params != null) {
            error(annotation, Diagnostic.Kind.IncompleteType(type.params.size))
        }
        return type
    }

    private fun resolveThisType(node: HasLocation): Type {
        val interfaceDecl = ctx.resolver.getEnclosingInterfaceDecl(node)
        return if (interfaceDecl != null) {
            Type.ThisRef(interfaceDecl.location)
        } else {
            // TODO: Allow implement defs to refer to This type
            error(node, Diagnostic.Kind.UnboundThisType)
            Type.Error
        }
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

    public fun typeOfEnumInstance(declaration: Declaration.Enum): Type {
        val name = ctx.resolver.qualifiedEnumName(declaration)
        val typeParams = declaration.typeParams?.map { Type.Param(it.binder) }
        val constructor = Type.Constructor(declaration.name, name, typeParams)
        return if (typeParams != null) {
            Type.Application(constructor, typeParams.map { Type.ParamRef(it.binder) })
        } else {
            constructor
        }
    }

    fun typeOfStructInstance(declaration: Declaration.Struct): Type {
        val constructorType = typeOfStructConstructor(declaration)
        require(constructorType is Type.Function)
        return constructorType.to
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
        is Declaration.Interface -> checkInterfaceDeclaration(declaration)
        is Declaration.Implementation -> checkImplementationDeclaration(declaration)
        is Declaration.Enum -> checkEnumDeclaration(declaration)
    }

    private fun checkEnumDeclaration(declaration: Declaration.Enum) {
        val variantNames = mutableListOf<Name>()
        for (case in declaration.cases) {
            for (param in case.params) {
                inferAnnotation(param)
            }
            if (case.name.identifier.name in variantNames) {
                error(case.name, Diagnostic.Kind.DuplicateVariantName)
            } else {
                variantNames.add(case.name.identifier.name)
            }
        }
    }

    private fun checkImplementationDeclaration(declaration: Declaration.Implementation) {

        val interfaceRef = declaration.interfaceRef
        checkInterfaceRef(interfaceRef)
        val interfaceDef = ctx.resolver.resolveDeclaration(interfaceRef.path)
        if (interfaceDef == null || interfaceDef !is Declaration.Interface) {
            return
        }

        for (member in declaration.members) {
            exhaustive(when(member) {
                is Declaration.Implementation.Member.FunctionDef -> {
                    checkFunctionDef(member.functionDef)
                }
            })
        }
    }

    private fun checkInterfaceRef(interfaceRef: InterfaceRef) {
        val interfaceDef = ctx.resolver.resolveDeclaration(interfaceRef.path)
        if (interfaceDef == null || interfaceDef !is Declaration.Interface) {
            error(interfaceRef.path, Diagnostic.Kind.NotAnInterface)
        }
        interfaceRef.typeArgs?.forEach {
            inferAnnotation(it)
        }
    }

    private fun checkInterfaceDeclaration(declaration: Declaration.Interface) {
        val name = ctx.resolver.qualifiedInterfaceName(declaration)
        interfaceDecls[name] = declaration
        for (member in declaration.members) {
            exhaustive(when (member) {
                is Declaration.Interface.Member.FunctionSignature -> typeOfFunctionSignature(member.signature)
            })
        }
    }

    private fun checkConstDef(declaration: Declaration.ConstDefinition) {
        declareGlobalConst(declaration)
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        val functionType = typeOfFunctionSignature(declaration.signature)
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

    fun typeOfFunctionSignature(signature: FunctionSignature): Type.Function {
        val cached = binderTypes[signature.name]
        if (cached != null) {
            require(cached is Type.Function)
            return cached
        }
        val paramTypes = mutableListOf<Type>()
        for (param in signature.params) {
            val type = if (param.annotation != null) {
                inferAnnotation(param.annotation)
            } else {
                Type.Error
            }
            bindValue(param.binder, type)
            paramTypes.add(type)
        }
        val receiverType = if (signature.thisParam != null) {
            inferAnnotation(signature.thisParam.annotation)
        } else {
            null
        }
        val returnType = inferAnnotation(signature.returnType)
        val constraints = buildList {
            signature.typeParams?.forEach {
                if (it.bound != null) {
                    checkInterfaceRef(it.bound)
                    val interfaceName = resolveInterfaceName(it.bound)
                    if (interfaceName != null) {
                        add(Type.Constraint(
                                interfaceName = interfaceName,
                                args = it.bound.typeArgs?.map { arg -> inferAnnotation(arg) } ?: listOf(),
                                param = Type.Param(it.binder)
                        ))
                    }
                }
            }
        }
        val type = Type.Function(
                receiver = receiverType,
                from = paramTypes,
                to = returnType,
                typeParams = signature.typeParams?.map { inferTypeParam(it) },
                constraints = constraints
        )
        bindValue(signature.name, type)
        return type
    }

    private fun resolveInterfaceName(interfaceRef: InterfaceRef): QualifiedName? {
        val decl = ctx.resolver.resolveDeclaration(interfaceRef.path)
        if (decl == null || decl !is Declaration.Interface) {
            error(interfaceRef.path.location, Diagnostic.Kind.NotAnInterface)
            return null
        }
        return ctx.resolver.qualifiedInterfaceName(decl)
    }

    private fun inferTypeParam(it: TypeParam): Type.Param {
        // FIXME: Infer bound
        return Type.Param(it.binder)
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
        is Statement.Error -> {
        }
        is Statement.LocalAssignment -> checkLocalAssignment(statement)
    }

    private fun checkLocalAssignment(statement: Statement.LocalAssignment) {
        val binding = ctx.resolver.resolve(statement.name)
        return when (binding) {
            is ValueBinding.ValBinding -> {
                if (!binding.statement.isMutable) {
                    error(statement.name, Diagnostic.Kind.AssignmentToImmutableVariable)
                }
                checkValStatement(binding.statement)
                val type = requireNotNull(binderTypes[binding.statement.binder])
                checkExpression(type, statement.value)

            }
            null -> {
                error(statement.name, Diagnostic.Kind.UnboundVariable)
            }
            else -> {
                // TODO: Show a more helpful diagnostic here
                error(statement.name, Diagnostic.Kind.NotAnAddressableValue)
            }
        }
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

    private fun inferExpression(expression: Expression, typeArgs: List<TypeAnnotation>? = null): Type {
        val ty = when (expression) {
            is Expression.Error -> Type.Error
            is Expression.Var -> inferVar(expression)
            is Expression.Call -> inferCall(expression)
            is Expression.Property -> inferProperty(expression, typeArgs)
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
            is Expression.BinaryOperation -> {
                if (expression.operator == op.EQUALS || expression.operator == op.NOT_EQUALS) {
                    val lhsTy = inferExpression(expression.lhs)
                    checkExpression(lhsTy, expression.rhs)

                    if (!doesTypeAllowEqualityComparison(lhsTy)) {
                        error(expression.location, Diagnostic.Kind.TypeNotEqualityComparable(lhsTy))
                    }

                    Type.Bool
                } else {
                    val lhsType = inferExpression(expression.lhs)
                    val rule = BIN_OP_RULES[expression.operator to lhsType]
                    if (rule == null) {
                        inferExpression(expression.rhs)
                        error(expression, Diagnostic.Kind.OperatorNotApplicable(expression.operator))
                        Type.Error
                    } else {
                        val (rhsTy, retTy) = rule
                        checkExpression(rhsTy, expression.rhs)
                        retTy
                    }
                }
            }
            is Expression.SizeOf -> {
                inferAnnotation(expression.type)
                Type.Size
            }
            is Expression.AddressOf -> {
                val ty = inferExpression(expression.expression)
                checkLValue(expression.expression)
                Type.RawPtr(ty)
            }
            is Expression.Load -> {
                val ty = inferExpression(expression.expression)
                if (ty is Type.RawPtr) {
                    ty.to
                } else {
                    error(expression.expression, Diagnostic.Kind.NotAPointerType(ty))
                    Type.Error
                }
            }
            is Expression.PointerCast -> {
                val toPtrOfType = inferAnnotation(expression.toType)
                val argTy = inferExpression(expression.arg)
                if (argTy !is Type.RawPtr) {
                    error(expression, Diagnostic.Kind.NotAPointerType(argTy))
                }
                Type.RawPtr(toPtrOfType)
            }
            is Expression.If -> {
                checkExpression(Type.Bool, expression.condition)
                val lhsType = inferExpression(expression.trueBranch)
                checkExpression(lhsType, expression.falseBranch)
                lhsType
            }
            is Expression.TypeApplication -> inferTypeApplicationExpression(expression)
            is Expression.Match -> inferMatchExpression(expression)
        }
        expressionTypes[expression] = ty
        return ty
    }

    private fun inferMatchExpression(expression: Expression.Match): Type {
        val valueType = inferExpression(expression.value)
        var typeArgs = listOf<Type>()
        val typeConstructor = when (valueType) {
            is Type.Constructor -> {
                valueType
            }
            is Type.Application -> {
                typeArgs = valueType.args
                valueType.callee
            }
            else -> {
                error(expression.value, Diagnostic.Kind.ExpectedEnumType)
                null
            }
        }
        if (typeConstructor == null)  {
            for (arm in expression.arms) {
                inferExpression(arm.expression)
            }
            return Type.Error
        }
        val decl = ctx.resolver.resolveDeclaration(typeConstructor.name)
        if (decl !is Declaration.Enum) {
            error(expression.value, Diagnostic.Kind.ExpectedEnumType)
            return Type.Error
        }

        val typeParams = decl.typeParams ?: listOf()
        val subst = typeParams.zip(typeArgs).map {
            it.first.binder.location to it.second
        }.toMap()

        val variantSet = decl.cases.map { it.name.identifier.name }
        val matchedVariants = mutableSetOf<Name>()
        var elseEncountered = false
        var armType: Type? = null

        for (arm in expression.arms) {
            val pattern = arm.pattern
            val armExpression = arm.expression
            if (elseEncountered) {
                error(pattern, Diagnostic.Kind.UnreachablePattern)
            }
            exhaustive(when (pattern) {
                is Pattern.DotName -> {
                    if (pattern.identifier.name !in variantSet) {
                        error(pattern, Diagnostic.Kind.UnboundPattern)
                    }

                    if (pattern.identifier.name in matchedVariants) {
                        error(pattern, Diagnostic.Kind.UnreachablePattern)
                    }


                    val case = decl.cases.find { it.name.identifier.name == pattern.identifier.name }

                    if (case != null) {
                        if (pattern.params.size != case.params.size) {
                            error(pattern, Diagnostic.Kind.PatternParamMismatch)
                        }
                        case.params.zip(pattern.params).forEach { (paramType, pattern) ->
                            if (pattern is Pattern.Name) {
                                binderTypes[pattern.binder] = inferAnnotation(paramType).applySubstitution(subst)
                            }
                        }
                    }

                    for (param in pattern.params) {
                        if (param !is Pattern.Name && param !is Pattern.Else) {
                            error(pattern, Diagnostic.Kind.NestedPatternsNotAllowed)
                        }
                    }

                    if (armType != null) {
                        checkExpression(armType, armExpression)
                    } else {
                        armType = inferExpression(armExpression)
                    }
                    matchedVariants.add(pattern.identifier.name)
                }
                is Pattern.Name -> {
                    binderTypes[pattern.binder] = valueType
                    if (armType != null)  {
                        checkExpression(armType, armExpression)
                    } else {
                        armType = inferExpression(armExpression)
                    }
                    elseEncountered = true
                }
                is Pattern.Else -> elseEncountered = true
            })
            if (elseEncountered) {
                matchedVariants.addAll(variantSet)
            }
        }

        if (!matchedVariants.containsAll(variantSet)) {
            error(expression.location, Diagnostic.Kind.NonExhaustivePatterns)
        }

        return armType ?: Type.Error
    }

    private fun inferTypeApplicationExpression(expression: Expression.TypeApplication): Type {
        val lhsType = inferExpression(expression.lhs)
        val typeParams = if (lhsType is Type.Constructor && lhsType.params != null) {
            lhsType.params
        } else {
            listOf()
        }
        if (lhsType !is Type.Constructor) {
            error(expression, Diagnostic.Kind.TooFewTypeArgs)
            return Type.Error
        }
        if (expression.args.size > typeParams.size) {
            error(expression, Diagnostic.Kind.TooManyTypeArgs)
            return Type.Error
        }

        if (expression.args.size < typeParams.size) {
            error(expression, Diagnostic.Kind.TooManyTypeArgs)
            return Type.Error
        }

        val typeArgs = expression.args.map { inferAnnotation(it) }

        return Type.Application(
                lhsType,
                typeArgs
        )
    }

    private fun checkLValue(expression: Expression) {

        if (expression !is Expression.Var &&
                !(expression is Expression.Property && expression.lhs is Expression.Var)) {
            error(expression, Diagnostic.Kind.NotAnAddressableValue)
        } else {
            val name = if (expression is Expression.Var) {
                expression.name
            } else if (expression is Expression.Property && expression.lhs is Expression.Var) {
                expression.lhs.name
            } else {
                null
            }
            if (name != null) {
                val binding = ctx.resolver.resolve(name)
                if (binding !is ValueBinding.ValBinding) {
                    error(expression, Diagnostic.Kind.NotAnAddressableValue)
                }
            }
        }
    }

    private fun doesTypeAllowEqualityComparison(type: Type): Boolean {
        return type is Type.Bool || type is Type.CInt || type is Type.Byte || type is Type.RawPtr
                || type is Type.Size
    }

    private fun inferThis(expression: Expression.This): Type {
        val thisParam = ctx.resolver.resolveThisParam(expression)
        if (thisParam == null) {
            error(expression, Diagnostic.Kind.UnboundThis)
            return Type.Error
        }
        return inferAnnotation(thisParam.annotation)
    }

    fun getPropertyBinding(expression: Expression.Property, typeArgs: List<TypeAnnotation>? = null): PropertyBinding? {
        val globalBinding = ctx.resolver.resolveModuleProperty(expression)
        if (globalBinding != null) {
            return PropertyBinding.Global(inferBinding(globalBinding), globalBinding)
        }
        val structFieldBinding = getStructFieldBinding(expression)
        if (structFieldBinding != null) {
            return structFieldBinding
        }
        val extensionMethodBinding = getGlobalExtensionFunctionBinding(expression)
        if (extensionMethodBinding != null) {
            return extensionMethodBinding
        }

        val interfaceExtensionBinding = getInterfacePropertyBinding(expression, typeArgs)
        if (interfaceExtensionBinding != null) {
            return interfaceExtensionBinding
        }

        return null
    }

    private fun getStructFieldBinding(expression: Expression.Property): PropertyBinding? {
        return when (val lhsType = inferExpression(expression.lhs)) {
            is Type.Application -> {
                getStructFieldBindingForTypeConstructor(lhsType.callee, lhsType.args, expression.property)
            }
            is Type.Constructor -> {
                getStructFieldBindingForTypeConstructor(lhsType, null, expression.property)
            }
            else -> null
        }
    }

    private fun inferProperty(expression: Expression.Property, typeArgs: List<TypeAnnotation>?): Type {
        return when (val binding = getPropertyBinding(expression, typeArgs)) {
            null -> {
                val lhsType = inferExpression(expression.lhs, typeArgs)
                error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
                Type.Error
            }
            else -> binding.type
        }
    }

    private fun getInterfaceImplementation(type: Type, node: HasLocation, interfaceName: QualifiedName, interfaceArgs: List<Type>): ImplementationBinding? {
        val implementations = getImplementationBindingsForType(type, atNode = node).toList()
        val actualInterfaceDef = ctx.resolver.resolveDeclaration(interfaceName) ?: return null
        if (actualInterfaceDef !is Declaration.Interface) {
            return null
        }
        val foundInstances = mutableListOf<ImplementationBinding>()
        for (implRef in implementations) {
            val interfaceRef = implRef.interfaceRef
            val interfaceDef = ctx.resolver.resolveDeclaration(interfaceRef.path) ?: continue
            if (interfaceDef !is Declaration.Interface) {
                continue
            }
            if (interfaceDef.location != actualInterfaceDef.location) {
                continue
            }
            val implementationTypeArgs = interfaceRef.typeArgs
                    ?.map { inferAnnotation(it) }
                    ?: listOf()
            if (implementationTypeArgs.size != interfaceArgs.size) {
                continue
            }

            val typeArgsMatch = implementationTypeArgs.zip(interfaceArgs).all { (actual, expected) ->
                isAssignableTo(destination = expected, source = actual)
            }

            if (typeArgsMatch) {
                foundInstances.add(implRef)
            }

        }
        require(foundInstances.size < 2) {
            "Overlapping instances"
        }
        return foundInstances.firstOrNull()
    }

    private fun getInterfacePropertyBinding(expression: Expression.Property, typeArgs: List<TypeAnnotation>?): PropertyBinding? {
        require(typeArgs == null) {
            TODO("Generic methods within interfaces not implemented")
        }
        val lhsType = inferExpression(expression.lhs, typeArgs)
        val property = expression.property
        val implementations = getImplementationBindingsForType(lhsType, atNode = expression).toList()
        for (implRef in implementations) {
            val interfaceRef = implRef.interfaceRef
            val interfaceDef = ctx.resolver.resolveDeclaration(interfaceRef.path) ?: continue
            if (interfaceDef !is Declaration.Interface) {
                continue
            }
            val implementationTypeArgs = interfaceRef.typeArgs
                ?.map { inferAnnotation(it) }
                ?: listOf()
            val substitution = interfaceDef.typeParams
                ?.map { it.binder.location }
                ?.zip(implementationTypeArgs)
                ?.toMap()
                ?: emptyMap()
            var memberIndex = -1
            memberLoop@ for (member in interfaceDef.members) {
                memberIndex++
                val memberType = when (member) {
                    is Declaration.Interface.Member.FunctionSignature -> {

                        if (member.signature.name.identifier.name != property.name) {
                            continue@memberLoop
                        }
                        val functionType = typeOfFunctionSignature(member.signature)
                        if (functionType.receiver == null) {
                            null
                        } else {
                            functionType.applySubstitution(
                                substitution,
                                lhsType
                            )
                        }
                    }
                }

                if (memberType != null) {
                    return PropertyBinding.InterfaceExtensionFunction(memberType, implRef, memberIndex)
                }
            }
        }
        return null
    }

    private fun getImplementationBindingsForType(lhsType: Type, atNode: HasLocation): Collection<ImplementationBinding> = sequence {
        for (implementation in ctx.resolver.implementationsInScope(atNode)) {
            val interfaceDecl = ctx.resolver.resolveDeclaration(implementation.interfaceRef.path)
            if (interfaceDecl !is Declaration.Interface) {
                continue
            }
            val forType = inferAnnotation(implementation.forType)
            if (isAssignableTo(source = lhsType, destination = forType)) {
                yield(ImplementationBinding.GlobalImpl(implementation))
            }
        }
        if (lhsType is Type.ParamRef) {
            val typeBinding = ctx.resolver.resolveTypeVariable(lhsType.name.identifier)
            require(typeBinding is TypeBinding.TypeParam)
            val bound = typeBinding.bound
            if (bound != null) {
                val functionDef = requireNotNull(ctx.resolver.getEnclosingFunction(lhsType.name))
                val typeParamIndex = functionDef.typeParams?.indexOfFirst {
                    it.binder.location == lhsType.name.location
                }
                require(typeParamIndex != null && typeParamIndex > -1)

                yield(ImplementationBinding.TypeBound(bound, functionDef, typeParamIndex))
            }

        }
    }.toList()

    private fun getStructFieldBindingForTypeConstructor(
            typeConstructor: Type.Constructor,
            typeArgs: List<Type>?,
            property: Identifier
    ): PropertyBinding? {
        val binder = requireNotNull(typeConstructor.binder)
        return when (val binding = ctx.resolver.resolveTypeVariable(binder.identifier)) {
            is TypeBinding.Struct -> {
                val field = binding.declaration.members.find {
                    it is Declaration.Struct.Member.Field && it.binder.identifier.name == property.name
                } as Declaration.Struct.Member.Field?

                if (field == null) {
                    return null
                }

                val fieldType = inferAnnotation(field.typeAnnotation)

                val substitution = binding.declaration.typeParams?.mapIndexed { index, it ->
                    it.binder.location to (typeArgs ?: listOf()).elementAtOrElse(index) { Type.Error }
                }?.toMap() ?: mapOf()

                PropertyBinding.StructField(
                        type = fieldType.applySubstitution(substitution),
                        structDecl = binding.declaration,
                        member = field
                )
            }
            else -> {
                null
            }
        }

            }

    private val extensionDefs = MutableNodeMap<Expression.Property, Declaration.FunctionDef>()

    private fun getGlobalExtensionFunctionBinding(expression: Expression.Property): PropertyBinding? {
        val lhsType = inferExpression(expression.lhs)
        val property = expression.property
        val extensionDefs = ctx.resolver.extensionDefsInScope(property, property)
        for (def in extensionDefs) {
            require(def.thisParam != null)
            val thisParamType = inferAnnotation(def.thisParam.annotation)
            if (isTypeEqual(thisParamType, lhsType)) {
                this.extensionDefs[expression] = def
                return PropertyBinding.GlobalExtensionFunction(typeOfBinder(def.name), def)
            }
            val typeParams = def.typeParams
            if (typeParams != null) {
                val substitution = mutableMapOf<SourceLocation, Type.GenericInstance>()
                typeParams.forEach {
                    substitution[it.binder.location] = makeGenericInstance(it.binder)
                }
                val functionType = typeOfBinder(def.name)

                val thisParamTypeInstance = thisParamType.applySubstitution(substitution)

                if (isAssignableTo(source = lhsType, destination = thisParamTypeInstance)) {
                    this.extensionDefs[expression] = def
                    return PropertyBinding.GlobalExtensionFunction(functionType, def)
                }
            }
        }
        return null
    }


    private fun makeGenericInstance(binder: Binder): Type.GenericInstance {
        _nextGenericInstance++
        return Type.GenericInstance(binder, _nextGenericInstance)
    }

    private val implBindings = MutableNodeMap<Expression.Call, List<ImplementationBinding>>()
    private fun inferCall(expression: Expression.Call, expectedReturnType: Type? = null): Type {
        val calleeType = inferExpression(expression.callee, expression.typeArgs)
        val functionType = if (calleeType is Type.Function) {
            calleeType
        } else if (calleeType is Type.RawPtr && calleeType.to is Type.Function) {
            calleeType.to
        } else {
            error(expression.location, Diagnostic.Kind.TypeNotCallable(calleeType))
            Type.Error
        }
        if (functionType is Type.Function) {
            val substitution = mutableMapOf<SourceLocation, Type>()
            functionType.typeParams?.forEach {
                substitution[it.binder.location] = makeGenericInstance(it.binder)
            }
            if (functionType.typeParams != null && expression.typeArgs != null) {
                functionType.typeParams.zip(expression.typeArgs).forEach { (typeParam, typeArg) ->
                    substitution[typeParam.binder.location] = inferAnnotation(typeArg)
                }
                if (expression.typeArgs.size > functionType.typeParams.size) {
                    error(expression.location, Diagnostic.Kind.TooManyTypeArgs)
                }
            }
            val len = min(functionType.from.size, expression.args.size)
            val to = functionType.to.applySubstitution(substitution)
            if (expectedReturnType != null) {
                checkAssignability(expression.location, source = to, destination = expectedReturnType)
            }
            if (functionType.receiver != null) {
                require(expression.callee is Expression.Property)
                val expected = functionType.receiver.applySubstitution(substitution)
                val found = expression.callee.lhs
                checkExpression(expected, found)
            }
            for (index in 0 until len) {
                val expected = functionType.from[index].applySubstitution(substitution)
                val found = expression.args[index].expression
                checkExpression(expected, found)
            }

            if (functionType.from.size > expression.args.size) {
                error(expression, Diagnostic.Kind.MissingArgs(required = functionType.from.size))
            } else if (functionType.from.size < expression.args.size) {
                error(expression, Diagnostic.Kind.TooManyArgs(required = functionType.from.size))
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
            functionType.typeParams?.forEach {
                val generic = requireNotNull(substitution[it.binder.location])
                val instance = if (generic is Type.GenericInstance)
                    genericInstantiations[generic.id]
                else generic
                typeArgs.add(
                        if (instance == null) {
                            error(expression.args.firstOrNull()
                                    ?: expression, Diagnostic.Kind.UninferrableTypeParam(it.binder))
                            Type.Error
                        } else {
                            instance
                        }
                )
            }
            val constraintBindings = mutableListOf<ImplementationBinding>()
            for (constraint in functionType.constraints) {
                val generic = requireNotNull(substitution[constraint.param.binder.location])
                val forType = if (generic is Type.GenericInstance)
                    genericInstantiations[generic.id]
                else generic
                if (forType != null) {
                    val interfaceTypeArgs = constraint.args.map { it.applySubstitution(substitution) }
                    val implBinding = getInterfaceImplementation(
                            forType,
                            expression,
                            constraint.interfaceName,
                            interfaceTypeArgs
                    )
                    if (implBinding == null) {
                        error(expression, Diagnostic.Kind.NoImplementationFound)
                    } else {
                        constraintBindings.add(implBinding)
                    }
                }
            }
            implBindings[expression] = constraintBindings
            if (functionType.typeParams != null) {
                typeArguments[expression] = typeArgs
                typeArguments[expression.callee] = typeArgs
            }
            return applyInstantiations(to)
        } else {
            for (arg in expression.args) {
                inferExpression(arg.expression)
            }
            if (functionType != Type.Error) {
                error(expression, Diagnostic.Kind.TypeNotCallable(functionType))
            }
            return Type.Error
        }
    }


    private fun applyInstantiations(type: Type): Type = when (type) {
        Type.Error,
        Type.Byte,
        Type.Void,
        Type.CInt,
        Type.Size,
        is Type.ParamRef,
        Type.Bool -> type
        is Type.RawPtr -> Type.RawPtr(type.to)
        is Type.Function -> Type.Function(
                receiver = if (type.receiver != null) applyInstantiations(type.receiver) else null,
                typeParams = type.typeParams,
                from = type.from.map { applyInstantiations(it) },
                to = applyInstantiations(type.to),
                constraints = type.constraints.map {
                    Type.Constraint(
                            interfaceName = it.interfaceName,
                            args = it.args.map { arg -> applyInstantiations(arg) },
                            param = it.param
                    )
                }
        )
        is Type.GenericInstance -> {
            genericInstantiations[type.id] ?: type
        }
        is Type.Application -> {
            Type.Application(
                    applyInstantiations(type.callee) as Type.Constructor,
                    type.args.map { applyInstantiations(it) }
            )
        }
        is Type.Constructor -> type
        is Type.ThisRef -> type
        is Type.UntaggedUnion -> Type.UntaggedUnion(type.members.map { applyInstantiations(it) })
    }

    private fun applyInstantiations(expression: Expression) {
        val ty = expressionTypes[expression] ?: inferExpression(expression)
        val instance = applyInstantiations(ty)
        expressionTypes[expression] = instance
    }

    private fun checkExpression(expected: Type, expression: Expression): Unit = when {
        expression is Expression.NullPtr && expected is Type.RawPtr -> {
            expressionTypes[expression] = expected
        }
        expression is Expression.IntLiteral && expected is Type.Size -> {
            expressionTypes[expression] = Type.Size
        }
        else -> {
            val exprType = inferExpression(expression, null)
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
        source is Type.Size && destination is Type.Size -> true
        source is Type.CInt && destination is Type.CInt -> true
        source is Type.Bool && destination is Type.Bool -> {
            true
        }
        source is Type.Byte && destination is Type.Byte -> {
            true
        }
        source is Type.Void && destination is Type.Void -> {
            true
        }
        source is Type.ParamRef && destination is Type.ParamRef
                && source.name.location == destination.name.location -> {
            true
        }
        source is Type.RawPtr && destination is Type.RawPtr ->
            isAssignableTo(source.to, destination.to)
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
        source is Type.GenericInstance -> {
            val sourceInstance = genericInstantiations[source.id]
            if (sourceInstance != null) {
                isAssignableTo(source = sourceInstance, destination = destination)
            } else {
                genericInstantiations[source.id] = destination
                true
            }
        }
        source is Type.Function && destination is Type.Function -> {
            require(source.receiver == null)
            require(destination.receiver == null)
            require(source.typeParams == null)
            require(destination.typeParams == null)
            var isEqual = true
            isEqual = isEqual && source.from.size == destination.from.size
            isEqual = isEqual && source.from.zip(destination.from).all { (source, destination) ->
                // Function type assignability is contravariant in parameter type
                // so source and destination types are reversed here
                isAssignableTo(source = destination, destination = source)
            }
            isEqual = isEqual && isAssignableTo(source = source.to, destination = destination.to)


            isEqual
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
            typeOfFunctionSignature(binding.declaration.signature)
            Type.RawPtr(requireNotNull(binderTypes[binding.declaration.name]))
        }
        is ValueBinding.ExternFunction -> {
            declareExternFunctionDef(binding.declaration)
            Type.RawPtr(requireNotNull(binderTypes[binding.declaration.binder]))
        }
        is ValueBinding.FunctionParam -> {
            typeOfFunctionSignature(binding.declaration.signature)
            requireNotNull(binderTypes[binding.param.binder])
        }
        is ValueBinding.ValBinding -> {
            checkValStatement(binding.statement)
            requireNotNull(binderTypes[binding.statement.binder])
        }
        is ValueBinding.Struct -> {
            declareStruct(binding.declaration)
            Type.RawPtr(requireNotNull(binderTypes[binding.declaration.binder]))
        }
        is ValueBinding.GlobalConst -> {
            declareGlobalConst(binding.declaration)
            requireNotNull(binderTypes[binding.declaration.name])
        }
        is ValueBinding.EnumCaseConstructor -> {
            typeOfEnumConstructor(binding.declaration, binding.case)
        }
        is ValueBinding.Pattern -> requireNotNull(binderTypes[binding.pattern.binder])
    }

    private fun typeOfEnumConstructor(
            declaration: Declaration.Enum,
            case: Declaration.Enum.Case
    ): Type {
        if (case.params.isEmpty()) {
            val typeParams = declaration.typeParams?.map {
                require(it.bound == null)
                Type.Param(it.binder)
            }
            return Type.Constructor(
                    binder = declaration.name,
                    name = ctx.resolver.qualifiedEnumName(declaration),
                    params = typeParams
            )
        } else {
            val instanceType = typeOfEnumInstance(declaration)
            val typeParams = declaration.typeParams?.map {
                require(it.bound == null)
                Type.Param(it.binder)
            }
            val from = case.params.map { inferAnnotation(it) }
            return Type.Function(
                    receiver = null,
                    constraints = emptyList(),
                    typeParams = typeParams,
                    from = from,
                    to = instanceType

            )
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
                typeParams = null, // extern functions can't be generic
                constraints = listOf()
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
            is Type.CInt -> {
            }
            is Type.Bool -> {
            }
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
        val instanceType = if (typeParams != null) {
            Type.Application(constructor, typeParams.map { Type.ParamRef(it.binder) })
        } else {
            constructor
        }
        val constructorParamTypes = fieldTypes.values.toList()
        val constructorType = Type.Function(
                from = constructorParamTypes,
                to = instanceType,
                typeParams = declaration.typeParams?.map { Type.Param(it.binder) },
                receiver = null,
                constraints = listOf()
        )
        bindValue(declaration.binder, constructorType)
    }

    private val interfaceDecls = mutableMapOf<QualifiedName, Declaration.Interface>()
    fun getInterfaceDecl(name: QualifiedName): Declaration.Interface {
        return requireNotNull(interfaceDecls[name])
    }

    fun getConstraintBindings(expression: Expression.Call): List<ImplementationBinding> {
        return requireNotNull(implBindings[expression])
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

