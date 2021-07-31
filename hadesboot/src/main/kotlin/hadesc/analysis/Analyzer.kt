package hadesc.analysis

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.frontend.PropertyBinding
import hadesc.ir.BinaryOperator
import hadesc.ir.passes.TypeTransformer
import hadesc.ir.passes.TypeVisitor
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.Binding
import hadesc.resolver.TypeBinding
import hadesc.types.Substitution
import hadesc.types.Type
import hadesc.unit
import java.util.*
import java.util.Collections.singletonList
import kotlin.math.min

class Analyzer(
        private val ctx: Context
) {
    val typeAnalyzer = TypeAnalyzer()
    private val returnTypeStack = Stack<Type?>()

    fun resolvePropertyBinding(expression: Expression.Property): PropertyBinding? {
        val modulePropertyBinding = ctx.resolver.resolveModuleProperty(expression)
        if (modulePropertyBinding != null) {
            return PropertyBinding.Global(modulePropertyBinding)
        }


        val traitFunctionRefBinding = resolveTraitFunctionRefBinding(expression)
        if (traitFunctionRefBinding != null) return traitFunctionRefBinding


        val sealedTypeCaseConstructorBinding = resolveSealedTypeConstructorBinding(expression)
        if (sealedTypeCaseConstructorBinding != null) {
            return sealedTypeCaseConstructorBinding
        }

        val matchArmBinding = resolveMatchArmPropertyBinding(expression)
        if (matchArmBinding != null) {
            return matchArmBinding
        }

        val fieldBinding = resolveStructFieldBinding(expression)
        if (fieldBinding != null) {
            return fieldBinding
        }

        val traitProperty = resolveTraitProperty(expression)
        if (traitProperty != null) {
            return traitProperty
        }

        val elementPointerBinding = resolveElementPointerBinding(expression)
        if (elementPointerBinding != null) {
            return elementPointerBinding
        }

        val extensionBinding = resolveExtensionBinding(expression)
        if (extensionBinding != null) {
            return extensionBinding
        }
        inferExpression(expression.lhs)
        return null
    }

    private fun resolveTraitFunctionRefBinding(expression: Expression.Property): PropertyBinding.TraitFunctionRef? {
        val traitDef = resolveTraitRef(expression.lhs) ?: return null
        val signature = traitDef.signatures.find { it.name.identifier.name == expression.property.name } ?: return null
        val typeArgs = if (expression.lhs is Expression.TypeApplication)
            expression.lhs.args.map { annotationToType(it) }
        else emptyList()
        val substitution = traitDef.params.zip(typeArgs)
            .associate { (param, arg) -> param.location to arg }
        check(signature.typeParams == null)
        return PropertyBinding.TraitFunctionRef(
            ctx.resolver.qualifiedName(traitDef.name),
            typeArgs,
            Type.Ptr(Type.Function(
                from = signature.params.map { it.type.applySubstitution(substitution) },
                to = annotationToType(signature.returnType).applySubstitution(substitution),
                traitRequirements = null
            ), isMutable = false),
            methodName = signature.name.name
        )
    }

    private val Param.type get(): Type = annotation?.let {
        annotationToType(it)
    } ?: Type.Error(location)

    private fun resolveSealedTypeConstructorBinding(expression: Expression.Property): PropertyBinding.SealedTypeCaseConstructor? {
        if (expression.lhs !is Expression.Var) {
            return null
        }
        val binding = ctx.resolver.resolve(expression.lhs.name)
        if (binding !is Binding.SealedType) return null
        val case = binding.declaration.cases.find { it.name.identifier.name == expression.property.name }
            ?: return null
        val type = typeOfSealedTypeCase(binding.declaration, case)
        return PropertyBinding.SealedTypeCaseConstructor(binding.declaration, case, type)
    }

    private fun resolveMatchArmPropertyBinding(expression: Expression.Property): PropertyBinding? {
        if (expression.lhs !is Expression.Var) return null
        val binding = ctx.resolver.resolve(expression.lhs.name)
        if (binding !is Binding.WhenArm) {
            return null
        }
        val lhs = expression.lhs
        return when (val case = binding.case) {
            is Expression.WhenArm.Is -> {
                val whenExpression = requireNotNull(ctx.resolver.getEnclosingWhenExpression(case.caseName))
                val discriminantType = inferExpression(whenExpression.value)
                val typeDecl = typeDeclarationOf(discriminantType)
                if (typeDecl !is Declaration.SealedType) {
                    return null
                }

                val sealedCase = typeDecl.cases.find {
                    it.name.identifier.name == case.caseName.name
                } ?: return null

                val paramIndex = sealedCase.params?.indexOfFirst { it.binder.identifier.name == expression.property.name }
                    ?: return null
                val param = sealedCase.params[paramIndex]
                val annotation = param.annotation ?: return null
                val annotationType = annotationToType(annotation)
                val typeArgs = discriminantType.typeArgs()
                val type = if (typeDecl.typeParams == null) {
                    annotationType
                } else {
                    val substitution = typeDecl.typeParams.zip(typeArgs).associate { (param, arg) ->
                        param.location to arg
                    }
                    annotationType.applySubstitution(substitution)
                }
                PropertyBinding.WhenCaseFieldRef(
                    typeDecl,
                    sealedCase.name.identifier.name,
                    typeArgs,
                    lhs.name,
                    paramIndex,
                    param.binder.identifier,
                    type,
                )
            }
            is Expression.WhenArm.Else -> requireUnreachable()
        }
    }

    private fun typeDeclarationOf(type: Type): Declaration? = when (type) {
        is Type.Constructor -> ctx.resolver.resolveDeclaration(type.name)
        is Type.Application -> typeDeclarationOf(type.callee)
        else -> null
    }

    private fun typeOfSealedTypeCase(
        declaration: Declaration.SealedType,
        case: Declaration.SealedType.Case
    ): Type {
        val typeConstructor = Type.Constructor(declaration.name, ctx.resolver.qualifiedName(declaration.name))
        val instanceType = if (declaration.typeParams == null) {
            typeConstructor
        } else {
            Type.Application(
                typeConstructor,
                declaration.typeParams.map { Type.ParamRef(it.binder) }
            )
        }

        val withArgs = if (case.params == null) {
            instanceType
        } else {
            Type.Ptr(Type.Function(
                from = case.params.map { if (it.annotation != null) annotationToType(it.annotation) else Type.Error(it.location) },
                to = instanceType,
                traitRequirements = null
            ), false)
        }
        return if (declaration.typeParams == null) {
            withArgs
        } else {
            Type.TypeFunction(
                params = declaration.typeParams.map { Type.Param(it.binder) },
                body = withArgs
            )
        }
    }

    private fun resolveTraitProperty(expression: Expression.Property): PropertyBinding? {
        if (expression.lhs !is Expression.Var) {
            return null
        }
        ctx.resolver.resolve(expression.lhs.name) ?: return null
        val lhsType = inferExpression(expression.lhs)
        val traitDef = getTraitDefOfType(lhsType) ?: return null
        val memberIndex = traitDef.signatures.indexOfFirst { it.name.identifier.name == expression.property.name }
        val memberSignature = traitDef.signatures[memberIndex]
        val traitTypeArgs = if (lhsType is Type.Application) lhsType.args else emptyList()
        val substitution = traitDef.params.zip(traitTypeArgs).associate {
            it.first.binder.location to it.second
        }
        val type = Type.Function(
                from = memberSignature.params.map {
                    it.annotation?.let { annot -> annotationToType(annot).applySubstitution(substitution) }
                            ?: Type.Error(it.location)

                },
                to = annotationToType(memberSignature.returnType).applySubstitution(substitution),
                traitRequirements = null
        )
        return PropertyBinding.WhereParamRef(
                traitDef,
                memberIndex,
                type = type
        )
    }

    private fun resolveExtensionBinding(expression: Expression.Property): PropertyBinding.ExtensionDef? {
        for (extensionDef in ctx.resolver.extensionDefsInScope(expression)) {
            if (null == extensionDef.declarations.filterIsInstance<Declaration.FunctionDef>().find { it.name.identifier.name == expression.property.name }) {
                continue
            }
            if (hasExtensionMethodForType(expression, extensionDef, expression.property.name, typeOfExpression(expression.lhs))) {
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
        for (functionDef in extensionDef.functionDefs) {
            index++
            if (functionDef.signature.thisParamFlags == null) {
                continue
            }
            if (expression.property.name != functionDef.name.identifier.name) {
                continue
            }

            val thisType = instantiateType(annotationToType(extensionDef.forType), extensionDef.typeParams)
            val expectedThisType = if (functionDef.signature.thisParamFlags.isPointer) {
                Type.Ptr(
                    thisType,
                    isMutable = functionDef.signature.thisParamFlags.isMutable
                )
            } else {
                thisType
            }
            if (!isTypeAssignableTo(expression, source = inferExpression(expression.lhs), destination = expectedThisType)) {
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
            var methodType: Type = Type.Ptr(Type.Function(
                from = listOf(receiverType) + functionDef.params.mapIndexed { paramIndex, _ ->
                    typeOfParam(functionDef, paramIndex)
                },
                to = annotationToType(functionDef.signature.returnType),
                traitRequirements = null
            ), isMutable = false)
            if (extensionDef.typeParams != null || functionDef.typeParams != null) {
                methodType = Type.TypeFunction(
                    ((extensionDef.typeParams ?: emptyList()) + (functionDef.typeParams ?: emptyList())).map { Type.Param(it.binder) },
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
        typeParams: List<TypeParam>?
    ): Pair<Type, Substitution> {
        if (typeParams == null) return type to emptyMap()
        val substitution = instantiateSubstitution(typeParams.map { Type.Param(it.binder) })
        return type.applySubstitution(substitution) to substitution
    }

    private fun hasExtensionMethodForType(callNode: HasLocation, extensionDef: Declaration.ExtensionDef, methodName: Name, type: Type): Boolean {
        val hasRequiredMethodName = extensionDef.declarations.filterIsInstance<Declaration.FunctionDef>()
            .any { it.name.name ==  methodName }
        if (!hasRequiredMethodName) return false
        val forType = annotationToType(extensionDef.forType)
        val valueAssignmentSubstitution = extensionDef.typeParams?.associate {
            it.location to typeAnalyzer.makeGenericInstance(
                it.binder
            )
        } ?: emptyMap()
        val pointerAssignmentSubstitution = extensionDef.typeParams?.associate {
            it.location to typeAnalyzer.makeGenericInstance(
                it.binder
            )
        } ?: emptyMap()
        val isValueAssignable = isTypeAssignableTo(
                callNode,
                source = type,
                destination = forType.applySubstitution(valueAssignmentSubstitution)
            )
        val isPointerAssignable = isTypeAssignableTo(
            callNode,
            source = type,
            destination = Type.Ptr(forType.applySubstitution(pointerAssignmentSubstitution), isMutable = false))

        if (!isValueAssignable && !isPointerAssignable) return false
        var assignableCount = 0
        if (isPointerAssignable) assignableCount++
        if (isValueAssignable) assignableCount++
        val methods = extensionDef.declarations.filterIsInstance<Declaration.FunctionDef>()
        val substitution = when {
            assignableCount > 1 -> {
                val pointerMethodAvailable = methods.find {
                    it.name.identifier.name == methodName && (it.signature.thisParamFlags?.isPointer ?: false)
                } != null
                val valueMethodAvailable = methods.find {
                    it.name.identifier.name == methodName && (it.signature.thisParamFlags != null && !it.signature.thisParamFlags.isPointer)
                }  != null

                var candidateCount = 0
                if (pointerMethodAvailable) candidateCount++
                if (valueMethodAvailable) candidateCount++

                when {
                    candidateCount > 1 -> {
                        TODO("Ambiguous extension method")
                    }
                    pointerMethodAvailable -> {
                        pointerAssignmentSubstitution
                    }
                    valueMethodAvailable -> {
                        valueAssignmentSubstitution
                    }
                    else -> requireUnreachable()
                }
            }
            isPointerAssignable -> {
                pointerAssignmentSubstitution
            }
            isValueAssignable -> {
                valueAssignmentSubstitution
            }
            else -> requireUnreachable()
        }

        return extensionDef.traitRequirements.all { requirement ->
            isTraitRequirementSatisfied(callNode, requirement.copy(
                arguments = requirement.arguments.map { arg -> arg.applySubstitution(substitution) }
            ))
        }

    }

    private fun instantiateType(type: Type, typeParams: List<TypeParam>?): Type {
        return instantiateTypeAndSubstitution(type, typeParams).first
    }


    private fun resolveElementPointerBinding(expression: Expression.Property): PropertyBinding.StructFieldPointer? {
        val lhsType = inferExpression(expression.lhs)
        if (lhsType !is Type.Ptr) {
            return null
        }
        val fieldBinding = resolveStructFieldBinding(lhsType.to, expression.property) ?: return null
        return PropertyBinding.StructFieldPointer(
                fieldBinding.structDecl,
                fieldBinding.memberIndex,
                type = fieldBinding.type
        )

    }

    private fun resolveStructFieldBinding(lhsType: Type, property: Identifier): PropertyBinding.StructField? {
        val structDecl = getStructDeclOfType(lhsType)
        return if (structDecl == null) null else {
            val index = structDecl.members.indexOfFirst {
                it is Declaration.Struct.Member.Field
                        && it.binder.identifier.name == property.name
            }
            if (index > -1) {
                val field = structDecl.members[index]
                require(field is Declaration.Struct.Member.Field)
                val typeArgs = if (lhsType is Type.Application) {
                    lhsType.args
                } else emptyList()
                val substitution = structDecl.typeParams?.zip(typeArgs)
                    ?.associate { it.first.binder.location to it.second }
                    ?: emptyMap()
                val fieldType = annotationToType(field.typeAnnotation).applySubstitution(substitution)
                isTypeAssignableTo(property, lhsType, fieldType)
                PropertyBinding.StructField(
                        structDecl = structDecl,
                        memberIndex = index,
                        type = reduceGenericInstances(fieldType)
                )
            } else null
        }
    }
    private fun resolveStructFieldBinding(expression: Expression.Property): PropertyBinding? {
        val lhsType = inferExpression(expression.lhs)
        return resolveStructFieldBinding(lhsType, expression.property)
    }

    private fun getStructDeclOfType(lhsType: Type): Declaration.Struct? {
        return when (lhsType) {
            is Type.Constructor -> {
                val decl = ctx.resolver.resolveDeclaration(lhsType.name)
                if (decl is Declaration.Struct) decl
                else null
            }
            is Type.Application -> {
                getStructDeclOfType(lhsType.callee)
            }
            else -> null
        }
    }
    private fun getTraitDefOfType(lhsType: Type): Declaration.TraitDef? {
        return when (lhsType) {
            is Type.Constructor -> {
                val decl = ctx.resolver.resolveDeclaration(lhsType.name)
                if (decl is Declaration.TraitDef) decl
                else null
            }
            is Type.Application -> {
                getTraitDefOfType(lhsType.callee)
            }
            else -> null
        }
    }

    private fun equateTypes(at: HasLocation, source: Type, destination: Type) {
        // isTypeAssignable is side effectful in the sense that it instantiates
        // Type.GenericInstance types for matching source and destination types
        isTypeAssignableTo(at, source, destination)
    }

    fun isTypeAssignableTo(at: HasLocation, source: Type, destination: Type): Boolean {
        return typeAnalyzer.isTypeAssignableTo(
            source = source.reduceSelectTypes(at),
            destination = destination.reduceSelectTypes(at)
        )
    }

    private fun Type.reduceSelectTypes(at: HasLocation): Type {
        return object : TypeTransformer {
            override fun lowerSelectType(type: Type.Select): Type {
                val traitResolver = makeTraitResolver(at)
                val implAndSubst = traitResolver.getImplementationClauseAndSubstitution(
                    type.traitName,
                    type.traitArgs
                ) ?: return super.lowerSelectType(type)
                val (impl, subst) = implAndSubst
                if (impl !is TraitClause.Implementation) {
                    return super.lowerSelectType(type)
                }
                val def = impl.def ?: return super.lowerSelectType(type)
                val typeAlias = def.body.filterIsInstance<Declaration.TypeAlias>()
                    .find { it.name.name == type.associatedTypeName }
                    ?: return super.lowerSelectType(type)
                require(typeAlias.typeParams == null)
                return annotationToType(typeAlias.rhs).applySubstitution(subst)
            }
        }.lowerType(this)
    }

    private val typeOfExpressionCache = MutableNodeMap<Expression, Type>()
    fun typeOfExpression(expression: Expression): Type {
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
        exhaustive(when (declaration) {
            is Declaration.FunctionDef -> visitFunctionDef(declaration)
            is Declaration.ConstDefinition -> visitConstDef(declaration)
            else -> Unit
        })
    }

    private val checkTraitRequirementCache = MutableNodeMap<TraitRequirementAnnotation, TraitRequirement?>()
    private fun checkTraitRequirement(requirement: TraitRequirementAnnotation): TraitRequirement? = checkTraitRequirementCache.getOrPut(requirement) {
        val declaration = ctx.resolver.resolveDeclaration(requirement.path)
        val args = requirement.typeArgs?.map {
            annotationToType(it)
        }
        if (declaration !is Declaration.TraitDef) {
            null
        } else {
            TraitRequirement(ctx.resolver.qualifiedName(declaration.name), args ?: listOf())
        }

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
        val returnType = annotationToType(def.signature.returnType)
        returnTypeStack.push(returnType)
        checkBlock(def.body)
        returnTypeStack.pop()

    }

    private fun checkBlock(block: Block) {
        for (member in block.members) {
            checkBlockMember(member)
        }
    }

    private fun checkBlockMember(member: Block.Member): Unit = when(member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> checkStatement(member.statement)
    }

    private fun checkStatement(statement: Statement): Unit = when(statement) {
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
        return when (val binding = ctx.resolver.resolve(statement.name)) {
            is Binding.ValBinding -> {
                visitValStatement(binding.statement)
                val type = typeOfBinding(binding)
                checkExpression(statement.value, type)
                Unit
            }
            else -> Unit
        }
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

    private fun checkAssignability(at: HasLocation, source: Type, destination: Type) {
        isTypeAssignableTo(at, destination = destination, source = source)
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
        if (expectedType != null) {
            checkExpression(statement.rhs, expectedType)
        } else {
            inferExpression(statement.rhs)
        }
    }

    private fun isPredicateOperator(operator: BinaryOperator): Boolean {
        return (isEqualityCheckingOperator(operator)
                || isOrderPredicateOperator(operator))
    }

    private fun isEqualityCheckingOperator(operator: BinaryOperator): Boolean {
        return (operator == BinaryOperator.EQUALS
                || operator == BinaryOperator.NOT_EQUALS)
    }

    private fun isOrderPredicateOperator(operator: BinaryOperator): Boolean {
        return (operator == BinaryOperator.GREATER_THAN
                || operator == BinaryOperator.GREATER_THAN_EQUAL
                || operator == BinaryOperator.LESS_THAN
                || operator == BinaryOperator.LESS_THAN_EQUAL)
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
                    checkAssignability(expression, source = type, destination = expectedType)
                    type
                }
                is Expression.Call -> {
                    checkCallExpression(expression, expectedType)
                }
                is Expression.NullPtr -> checkNullPtrExpression(expression, expectedType)
                is Expression.Closure -> checkOrInferClosureExpression(expression, expectedType)
                is Expression.If -> checkIfExpression(expression, expectedType)
                else -> {
                    val inferredType = inferExpression(expression)
                    checkAssignability(at = expression, source = inferredType, destination = expectedType)
                    inferredType
                }
            }
        }
        typeOfExpressionCache[expression] = type
        return type
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
                at = expression,
                source = annotationToType(expression.returnType), destination = functionTypeComponents.to)
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
                            param,
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
        return Type.Function(
            from = paramTypes,
            to = returnType,
            traitRequirements = null
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

    private fun isIntLiteralAssignable(type: Type): Boolean = when(type) {
        is Type.Size,
        is Type.Integral -> true
        is Type.FloatingPoint -> true
        else -> false
    }

    private fun inferExpression(expression: Expression): Type = typeOfExpressionCache.getOrPut(expression) {
        reduceGenericInstances(when (expression) {
            is Expression.Error -> Type.Error(expression.location)
            is Expression.Var -> inferVarExpresion(expression)
            is Expression.Call -> inferCallExpression(expression)
            is Expression.Property -> inferPropertyExpression(expression)
            is Expression.ByteString -> Type.Ptr(Type.Integral(8, isSigned = false), isMutable = false)
            is Expression.BoolLiteral -> Type.Bool
            is Expression.NullPtr -> inferNullPtrExpression(expression)
            is Expression.IntLiteral -> inferIntLiteral(expression)
            is Expression.Not -> inferNotExpression(expression)
            is Expression.BinaryOperation -> inferBinaryOperation(expression)
            is Expression.SizeOf -> inferSizeOfExpression(expression)
            is Expression.AddressOf -> inferAddressOf(expression)
            is Expression.AddressOfMut -> inferAddressOfMut(expression)
            is Expression.Deref -> inferDerefExpression(expression)
            is Expression.PointerCast -> inferPointerCast(expression)
            is Expression.If -> inferIfExpression(expression)
            is Expression.TypeApplication -> inferTypeApplication(expression)
            is Expression.This -> inferThisExpression(expression)
            is Expression.Closure -> checkOrInferClosureExpression(expression, expectedType = null)
            is Expression.UnsafeCast -> inferUnsafeCast(expression)
            is Expression.When -> inferWhenExpression(expression)
            is Expression.As -> {
                inferExpression(expression.lhs)
                annotationToType(expression.rhs)
            }
        })
    }

    private fun inferWhenExpression(expression: Expression.When): Type {
        inferExpression(expression.value)
        val firstArmExpr = expression.arms.firstOrNull() ?: return Type.Error(expression.location)
        val type = inferExpression(firstArmExpr.value)
        for (whenArm in expression.arms.drop(1)) {
            checkExpression(whenArm.value, type)
        }
        return type
    }

    private fun inferUnsafeCast(expression: Expression.UnsafeCast): Type {
        inferExpression(expression.value)
        return annotationToType(expression.toType)
    }

    private fun inferTypeApplication(expression: Expression.TypeApplication): Type {
        val lhsType = inferExpression(expression.lhs)
        return if (lhsType is Type.TypeFunction) {
            val substitution = lhsType.params.zip(expression.args).associate { (param, annotation) ->
                param.binder.location to annotationToType(annotation)
            }
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
        return Type.Size
    }

    private fun inferBinaryOperation(expression: Expression.BinaryOperation): Type {
        val lhsType = inferExpression(expression.lhs)
        val matchingRule = BIN_OP_RULES[expression.operator to lhsType]
        return if (matchingRule != null) {
            checkExpression(expression.rhs, matchingRule.first)
            matchingRule.second
        } else {
            if ((expression.operator == BinaryOperator.EQUALS || expression.operator == BinaryOperator.NOT_EQUALS)
                && isTypeEquatable(lhsType)
            ) {
                checkExpression(expression.rhs, if (lhsType is Type.Ptr) lhsType.copy(isMutable = false) else lhsType)
                Type.Bool
            } else {
                inferExpression(expression.rhs)
                Type.Error(expression.location)
            }
        }
    }

    private fun isTypeEquatable(lhsType: Type): Boolean = when(reduceGenericInstances(lhsType)) {
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
                is PropertyBinding.StructFieldPointer -> binding.type
                is PropertyBinding.ExtensionDef -> binding.type
                is PropertyBinding.WhereParamRef -> binding.type
                is PropertyBinding.SealedTypeCaseConstructor -> binding.type
                is PropertyBinding.WhenCaseFieldRef -> binding.type
                null -> Type.Error(expression.location)
                is PropertyBinding.TraitFunctionRef -> binding.type
            }

    private fun typeOfGlobalPropertyBinding(
            binding: PropertyBinding.Global): Type {
        return typeOfBinding(binding.binding)
    }

    private fun inferVarExpresion(expression: Expression.Var): Type {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> {
                Type.Error(expression.location)
            }
            else -> typeOfBinding(binding)
        }
    }

    private fun typeOfBinding(binding: Binding): Type = when(binding) {
        is Binding.GlobalFunction -> typeOfGlobalFunctionRef(binding.declaration)
        is Binding.ExternFunction -> typeOfExternFunctionRef(binding.declaration)
        is Binding.FunctionParam -> typeOfParam(binding.declaration, binding.index)
        is Binding.ValBinding -> typeOfValRef(binding)
        is Binding.Struct -> typeOfStructValueRef(binding)
        is Binding.GlobalConst -> typeOfGlobalConstBinding(binding)
        is Binding.ClosureParam -> typeOfClosureParam(binding)
        is Binding.SealedType -> requireUnreachable()
        is Binding.WhenArm -> requireUnreachable()
        is Binding.ExternConst -> typeOfExternConstBinding(binding)
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
        val constrType = Type.Constructor(
            binding.declaration.binder,
            name
        )
        val instanceType = if (binding.declaration.typeParams == null) {
            constrType
        } else {
            Type.Application(
                constrType,
                binding.declaration.typeParams.map { Type.ParamRef(it.binder) }
            )
        }
        val fieldTypes = structFieldTypes(binding.declaration).values.toList()
        val functionType =
            Type.Ptr(Type.Function(from = fieldTypes, to = instanceType, traitRequirements = null), isMutable = false)
        return if (binding.declaration.typeParams == null) {
            functionType
        } else {
            Type.TypeFunction(
                params = binding.declaration.typeParams.map { Type.Param(it.binder) },
                body = functionType
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
        return Type.Ptr(
                to = Type.Function(
                        from = declaration.paramTypes.map { annotationToType(it) },
                        to = annotationToType(declaration.returnType),
                        traitRequirements = null
                ),
                isMutable = false
        )
    }

    private fun typeOfGlobalFunctionRef(declaration: Declaration.FunctionDef): Type {
        val functionType = Type.Function(
                from = declaration.params.map { param ->
                    param.annotation?.let { annotationToType(it) } ?: Type.Error(param.location) },
                to = annotationToType(declaration.signature.returnType),
                traitRequirements = declaration.signature.whereClause
                        ?.traitRequirements?.mapNotNull { checkTraitRequirement(it) }
        )
        val typeParams = declaration.typeParams
        val functionPointerType = Type.Ptr(functionType, isMutable = false)
        return if (typeParams != null) {
            Type.TypeFunction(
                    params = typeParams.map { Type.Param(it.binder) },
                    body = functionPointerType
            )
        } else functionPointerType
    }

    data class FunctionTypeComponents(
            val from: List<Type>,
            val to: Type,
            val typeParams: List<Type.Param>?,
            val traitRequirements: List<TraitRequirement>?
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
                functionType.typeParams,
            )

            if (functionType.typeParams != null) {
                val genericCallee = when (callee) {
                    is Expression.TypeApplication -> callee.lhs
                    else -> callee
                }
                getTypeArgsCache[genericCallee] = functionType.typeParams.map {
                    requireNotNull(substitution[it.binder.location])
                }
            }
            val receiver = getCallReceiver(callNode)
            val argsWithReceiver = if (receiver == null)
                args.map { it.expression }
            else listOf(receiver) + args.map { it.expression }
            val explicitTypeArgs = when (callee) {
                is Expression.TypeApplication -> callee.args.map { annotationToType(it) }
                else -> null
            }
            checkCallArgs(callNode, functionType, explicitTypeArgs, argsWithReceiver, substitution)
            if (expectedReturnType != null) {
                val source = functionType.to.applySubstitution(substitution)
                checkAssignability(
                        callNode,
                        source = source,
                        destination = expectedReturnType
                )
            }

            reduceGenericInstances(functionType.to.applySubstitution(substitution))
        }

    }

    private val globalTraitClauses: List<TraitClause<Declaration.ImplementationDef>> by lazy {
        ctx.resolver.implementationDefs.mapNotNull { implementationDef ->
            val traitDef = ctx.resolver.resolveDeclaration(implementationDef.traitRef)
            if (traitDef !is Declaration.TraitDef) {
                null
            } else {
                TraitClause.Implementation(
                        def = implementationDef,
                        params = implementationDef.typeParams?.map { p -> Type.Param(p.binder) } ?: emptyList(),
                        traitRef = ctx.resolver.qualifiedName(traitDef.name),
                        arguments = implementationDef.traitArguments.map { annotationToType(it) },
                        requirements = implementationDef.whereClause?.traitRequirements?.mapNotNull { checkTraitRequirement(it) } ?: emptyList()
                )
            }
        }
    }

    fun isTraitRequirementSatisfied(callNode: HasLocation, requiredInstance: TraitRequirement): Boolean {
        val traitResolver = makeTraitResolver(callNode)
        return traitResolver.isTraitImplemented(requiredInstance.traitRef, requiredInstance.arguments)
    }

    private fun makeTraitResolver(callNode: HasLocation): TraitResolver<Declaration.ImplementationDef> {
        val enclosingFunction = requireNotNull(ctx.resolver.getEnclosingFunction(callNode))
        val enclosingExtensionDef = ctx.resolver.getEnclosingExtensionDef(callNode)
        val enclosingImpl = ctx.resolver.getEnclosingImpl(callNode)
        val extensionClauses = enclosingExtensionDef?.traitRequirements?.map { it.toClause() } ?: emptyList()
        val functionTraitClauses = enclosingFunction.traitRequirements.map { it.toClause() }
        val implTraitClauses = enclosingImpl?.traitRequirements?.map { it.toClause() } ?: emptyList()
        val env = TraitResolver.Env(extensionClauses + functionTraitClauses + implTraitClauses + globalTraitClauses)
        return TraitResolver(env, typeAnalyzer)
    }

    private fun TraitRequirement.toClause(): TraitClause<Declaration.ImplementationDef> {
        return TraitClause.Requirement(this)
    }

    private val Declaration.FunctionDef.traitRequirements get(): List<TraitRequirement> =
        signature.whereClause
            ?.traitRequirements?.mapNotNull { checkTraitRequirement(it) }
            ?: emptyList()

    private val Declaration.ExtensionDef.traitRequirements get(): List<TraitRequirement> =
        whereClause?.traitRequirements?.mapNotNull { checkTraitRequirement(it) } ?: emptyList()

    private val Declaration.ImplementationDef.traitRequirements get(): List<TraitRequirement> =
        whereClause
                ?.traitRequirements
                ?.mapNotNull { checkTraitRequirement(it) }
                ?: emptyList()


    fun getCallReceiver(callNode: Expression.Call): Expression? = when(callNode.callee) {
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
        typeParams: List<Type.Param>?,
    ): Substitution {
        return typeParams?.associate {
            it.binder.location to makeGenericInstance(
                it.binder
            )
        }
            ?: emptyMap()
    }

    private fun checkCallArgs(
            callNode: HasLocation,
            functionType: FunctionTypeComponents,
            typeArgs: List<Type>?,
            args: List<Expression>,
            substitution: Substitution
    ) {
        val length = min(functionType.from.size, args.size)
        typeArgs?.zip(functionType.typeParams ?: emptyList())?.forEach { (arg, param) ->
            val expectedType = Type.ParamRef(param.binder).applySubstitution(substitution)
            equateTypes(at = callNode, source = arg, destination = expectedType)
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
                is Type.Ptr -> recurse(type.to, typeParams)
                is Type.Function ->
                    FunctionTypeComponents(
                            from = type.from,
                            to = type.to,
                            typeParams = typeParams,
                            traitRequirements = type.traitRequirements
                    )
                is Type.TypeFunction -> {
                    return recurse(type.body, type.params)
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


    private val annotationToTypeCache = MutableNodeMap<TypeAnnotation, Type>()
    fun annotationToType(annotation: TypeAnnotation): Type = annotationToTypeCache.getOrPut(annotation) {
        when (annotation) {
            is TypeAnnotation.Error -> Type.Error(annotation.location)
            is TypeAnnotation.Var -> varAnnotationToType(annotation)
            is TypeAnnotation.Ptr -> ptrAnnotationToType(annotation)
            is TypeAnnotation.MutPtr -> mutPtrAnnotationToType(annotation)
            is TypeAnnotation.Application -> typeApplicationAnnotationToType(annotation)
            is TypeAnnotation.Qualified -> qualifiedAnnotationToType(annotation)
            is TypeAnnotation.Function -> functionAnnotationToType(annotation)
            is TypeAnnotation.Union -> unionAnnotationToType(annotation)
            is TypeAnnotation.Select -> selectAnnotationToType(annotation)
        }
    }

    private fun selectAnnotationToType(annotation: TypeAnnotation.Select): Type {
        val traitResolver = makeTraitResolver(annotation)
        val requirement = asTraitRequirement(annotation.lhs) ?: return Type.Error(annotation.location)
        val clauseAndSubstitution = traitResolver.getImplementationClauseAndSubstitution(requirement.traitRef, requirement.arguments)
            ?: return Type.Error(annotation.location)
        val (clause, substitution) = clauseAndSubstitution
        if (clause is TraitClause.Requirement) {
            return Type.Select(clause.requirement.traitRef, clause.requirement.arguments, annotation.rhs.name)
        }
        require(clause is TraitClause.Implementation)
        val def = clause.def ?: return Type.Error(annotation.location)
        for (typeAlias in def.body.filterIsInstance<Declaration.TypeAlias>()) {
            require(typeAlias.typeParams == null)
            if (typeAlias.name.name == annotation.rhs.name) {
                return annotationToType(typeAlias.rhs).applySubstitution(substitution)
            }
        }
        return Type.Error(annotation.location)
    }

    private fun functionAnnotationToType(annotation: TypeAnnotation.Function): Type {
        return Type.Function(
            from = annotation.from.map { annotationToType(it) },
            to = annotationToType(annotation.to),
            traitRequirements = null
        )
    }

    private fun unionAnnotationToType(annotation: TypeAnnotation.Union): Type {
        return Type.UntaggedUnion(
            annotation.args.map { annotationToType(it) }
        )
    }

    private fun typeApplicationAnnotationToType(annotation: TypeAnnotation.Application): Type {
        val callee = annotationToType(annotation.callee)
        return if (callee is Type.TypeFunction) {
            val args = annotation.args.map { annotationToType(it) }
            val substitution = callee.params.zip(args).associate {
                it.first.binder.location to it.second
            }
            callee.body.applySubstitution(substitution)
        } else {
            Type.Error(annotation.location)
        }
    }

    private fun qualifiedAnnotationToType(annotation: TypeAnnotation.Qualified): Type {
        val binding = ctx.resolver.resolveQualifiedType(annotation.qualifiedPath)
        return if (binding == null) {
            Type.Error(annotation.location)
        } else {
            return typeOfTypeBinding(binding)
        }
    }

    private fun mutPtrAnnotationToType(annotation: TypeAnnotation.MutPtr): Type {
        return Type.Ptr(
                to = annotationToType(annotation.to),
                isMutable = true
        )
    }

    private fun ptrAnnotationToType(annotation: TypeAnnotation.Ptr): Type {
        return Type.Ptr(
                to = annotationToType(annotation.to),
                isMutable = false
        )
    }

    private fun typeOfTypeAlias(binding: TypeBinding.TypeAlias): Type {
        return if (binding.declaration.typeParams != null) {
            return Type.TypeFunction(
                    binding.declaration.typeParams.map { Type.Param(it.binder) },
                    annotationToType(binding.declaration.rhs)
            )
        }
        else annotationToType(binding.declaration.rhs)
    }

    private fun typeOfTraitTypeBinding(binding: TypeBinding.Trait): Type {
        val qualifiedName = ctx.resolver.qualifiedName(binding.declaration.name)
        val typeConstructor = Type.Constructor(binder = binding.declaration.name, name = qualifiedName)

        return Type.TypeFunction(
            params = binding.declaration.params.map { Type.Param(it.binder) },
            body = Type.Application(
                    typeConstructor,
                    args = binding.declaration.params.map { Type.ParamRef(it.binder) }
            )
        )
    }
    private fun typeOfStructBinding(binding: TypeBinding.Struct): Type {
        val qualifiedName = ctx.resolver.qualifiedStructName(binding.declaration)
        val typeConstructor = Type.Constructor(binder = binding.declaration.binder, name = qualifiedName)

        return if (binding.declaration.typeParams == null) {
            typeConstructor
        } else {
            Type.TypeFunction(
                    params = binding.declaration.typeParams.map { Type.Param(it.binder) },
                    body = Type.Application(
                            typeConstructor,
                            args = binding.declaration.typeParams.map { Type.ParamRef(it.binder) }
                    )
            )
        }
    }
    private fun typeOfSealedTypeBinding(binding: TypeBinding.SealedType): Type {
        val qualifiedName = ctx.resolver.qualifiedName(binding.declaration.name)
        val typeConstructor = Type.Constructor(binder = binding.declaration.name, name = qualifiedName)

        return if (binding.declaration.typeParams == null) {
            typeConstructor
        } else {
            Type.TypeFunction(
                params = binding.declaration.typeParams.map { Type.Param(it.binder) },
                body = Type.Application(
                    typeConstructor,
                    args = binding.declaration.typeParams.map { Type.ParamRef(it.binder) }
                )
            )
        }
    }

    private fun varAnnotationToType(annotation: TypeAnnotation.Var): Type {
        val resolved = resolveTypeVariable(annotation)
        return resolved ?: Type.Error(annotation.location)
    }

    private fun resolveTypeVariable(annotation: TypeAnnotation.Var): Type? {
        val binding = ctx.resolver.resolveTypeVariable(annotation.name) ?: return null
        return typeOfTypeBinding(binding)
    }

    private fun typeOfTypeBinding(binding: TypeBinding): Type {
        return when (binding) {
            is TypeBinding.Struct -> typeOfStructBinding(binding)
            is TypeBinding.TypeParam -> typeOfTypeParam(binding)
            is TypeBinding.TypeAlias -> typeOfTypeAlias(binding)
            is TypeBinding.Trait -> typeOfTraitTypeBinding(binding)
            is TypeBinding.SealedType -> typeOfSealedTypeBinding(binding)
            is TypeBinding.Builtin -> binding.type
            is TypeBinding.AssociatedType -> typeOfAssociatedTypeBinding(binding)
        }
    }

    private fun typeOfAssociatedTypeBinding(binding: TypeBinding.AssociatedType): Type {
        val traitDef = ctx.resolver.getEnclosingTraitDef(binding.binder)
        requireNotNull(traitDef)
        return Type.Select(
            ctx.resolver.qualifiedName(traitDef.name),
            traitArgs = traitDef.params.map { Type.ParamRef(it.binder) },
            associatedTypeName = binding.binder.name
        )
    }

    private fun typeOfTypeParam(binding: TypeBinding.TypeParam): Type {
        return Type.ParamRef(binding.binder)
    }

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
                return if (type.callee is Type.TypeFunction) {
                    require(type.callee.params.size == type.args.size)
                    val substitution =
                        type.callee.params.zip(type.args).associate { it.first.binder.location to it.second }
                    return type.callee.body.applySubstitution(substitution)
                } else {
                    super.lowerTypeApplication(type)
                }
            }
        }.lowerType(type)
    }

    fun getSealedTypeDeclaration(type: Type): Declaration.SealedType {
        val constructor = when (type) {
            is Type.Constructor -> type
            is Type.Application ->
                if (type.callee is Type.Constructor) {
                    type.callee
                } else {
                    requireUnreachable()
                }
            else -> requireUnreachable()
        }
        val declaration = ctx.resolver.resolveDeclaration(constructor.name)
        require(declaration is Declaration.SealedType)
        return declaration
    }
    fun getDiscriminants(type: Type): List<Discriminant> {
        val args = type.typeArgs()

        val declaration = getSealedTypeDeclaration(type)

        val substitution =
            (declaration.typeParams ?: emptyList()).zip(args).associate { (param, type) -> param.location to type }

        return declaration.cases.mapIndexed { index, case ->
            Discriminant(
                index,
                case.name.identifier.name,
                case.params?.map {
                    it.binder.identifier.name to
                            annotationToType(requireNotNull(it.annotation)).applySubstitution(substitution)
                } ?: singletonList(
                    ctx.makeName("dummy") to
                    Type.Integral(8, false)
                )
            )
        }
    }

    fun getSealedTypePayloadType(declaration: Declaration.SealedType): Type.UntaggedUnion {
        val sealedTypeName = ctx.resolver.qualifiedName(declaration.name)
        return Type.UntaggedUnion(declaration.cases.map {  case ->
            val constructorType = Type.Constructor(
                binder = null,
                sealedTypeName.append(case.name.identifier.name)
            )
            if (declaration.typeParams != null) {
                Type.Application(constructorType, declaration.typeParams.map { Type.ParamRef(it.binder) })
            } else {
                constructorType
            }
        })

    }

    fun isValidPropertyAccess(expression: Expression.Property): Boolean {
        typeOfExpression(expression)
        return resolvePropertyBinding(expression) != null
    }

    fun isCompileTimeConstant(initializer: Expression): Boolean {
        return when (initializer) {
            is Expression.IntLiteral,
            is Expression.BoolLiteral,
            is Expression.ByteString,
            is Expression.Var, -> true
            is Expression.Property -> {
                val binding = ctx.analyzer.resolvePropertyBinding(initializer)
                binding is PropertyBinding.Global
            }
            else -> {
                false
            }
        }
    }

    fun isSealedTypeConstructorCall(expression: Expression): Boolean = getSealedTypeConstructorBinding(expression) != null

    fun getSealedTypeConstructorBinding(expression: Expression): PropertyBinding.SealedTypeCaseConstructor? =
        when(expression) {
            is Expression.Property -> {
                val binding = resolvePropertyBinding(expression)
                if (binding is PropertyBinding.SealedTypeCaseConstructor) {
                    binding
                } else {
                    null
                }
            }
            is Expression.TypeApplication -> getSealedTypeConstructorBinding(expression.lhs)
            else -> null
        }

    fun getParamType(param: Param): Type {
        return reduceGenericInstances(requireNotNull(closureParamTypes[param.binder]))
    }

    fun getReturnType(expression: Expression): Type {
        val exprType = typeOfExpression(expression)
        return if (exprType is Type.Function) {
            reduceGenericInstances(exprType.to)
        } else {
            Type.Error(expression.location)
        }
    }

    private fun resolvePropertyExpressionTraitRef(expression: Expression.Property): Declaration.TraitDef? {
        if (expression.lhs !is Expression.Var) return null

        if (ctx.resolver.resolve(expression.lhs.name) != null) return null

        val moduleAlias = ctx.resolver.resolveModuleAlias(expression.lhs.name) ?: return null

        return ctx.resolver.findTraitInSourceFile(expression.property, moduleAlias)
    }

    fun resolveTraitRef(expression: Expression): Declaration.TraitDef? = when(expression) {
        is Expression.Var -> {
            ctx.resolver.resolveTraitDef(expression.name)
        }
        is Expression.Property -> {
            resolvePropertyExpressionTraitRef(expression)
        }
        is Expression.TypeApplication -> {
            resolveTraitRef(expression.lhs)
        }
        else -> null
    }

    fun getClosureCaptures(closure: Expression.Closure): ClosureCaptures {
        val values = mutableMapOf<Binder, Type>()
        val types = mutableSetOf<Binder>()
        object : SyntaxVisitor {
            val typeVisitor = object : TypeVisitor {
                override fun visitParamRefType(type: Type.ParamRef) {
                    if (!type.name.location.isWithin(closure.location)) {
                        types.add(type.name)
                    }
                }

                override fun visitTypeApplication(type: Type.Application) {
                    if (type.callee is Type.TypeFunction) {
                        val subst = type.callee.params.zip(type.args).associate {
                            it.first.binder.location to it.second
                        }
                        visitType(
                            type.callee.body.applySubstitution(subst)
                        )
                    }
                    else
                        super.visitTypeApplication(type)
                }
            }


            override fun visitExpression(expression: Expression) {
                val type = reduceGenericInstances(typeOfExpression(expression))

                val typeArgs = when (expression) {
                    is Expression.Call -> {
                        visitExpression(expression.callee)
                        expression.args.forEach {
                            visitExpression(it.expression)
                        }
                        getTypeArgs(expression.callee)

                    }
                    is Expression.TypeApplication -> {
                        expression.args.map { annotationToType(it) }
                    }
                    else -> {
                        super.visitExpression(expression)
                        null
                    }
                }
                if (type is Type.TypeFunction && typeArgs != null) {
                    val subst = type.params.zip(typeArgs).associate {
                        it.first.binder.location to it.second
                    }
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
                if (binding != null && !binding.isGlobal() && !binding.isLocalTo(closure)) {
                    values[binding.binder] = typeOfBinder(binding.binder)
                }
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

    fun asTraitRequirement(type: TypeAnnotation): TraitRequirement? = when(type) {
        is TypeAnnotation.Application -> {
            asTraitRequirement(type.callee)?.let { traitRequirement ->
                require(traitRequirement.arguments.isEmpty())
                traitRequirement.copy(
                    arguments = type.args.map { annotationToType(it) }
                )
            }
        }
        is TypeAnnotation.Var -> {
            val traitDef = ctx.resolver.resolveTraitDef(type.name)
            if (traitDef != null) {
                TraitRequirement(
                    ctx.resolver.qualifiedName(traitDef.name),
                    emptyList()
                )
            } else
                null
        }
        is TypeAnnotation.Qualified -> {
            ctx.resolver.resolveDeclaration(type.qualifiedPath)?.let {
                if (it is Declaration.TraitDef) {
                    TraitRequirement(
                        ctx.resolver.qualifiedName(it.name),
                        arguments = emptyList()
                    )
                } else {
                    null
                }
            }
        }
        else -> null
    }

    fun getTraitDef(traitRequirement: TraitRequirement): Declaration.TraitDef? {
        val declaration = ctx.resolver.resolveDeclaration(traitRequirement.traitRef)
        return if (declaration is Declaration.TraitDef) {
            declaration
        } else {
            null
        }
    }

}

data class ClosureCaptures(
    val values: Map<Binder, Type>,
    val types: Set<Binder>
)

private class MutableNodeMap<T : HasLocation, V> {
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
}

typealias op = BinaryOperator

val BIN_OP_RULES: Map<Pair<op, Type>, Pair<Type, Type>> = mapOf(

        (op.PLUS to Type.Size) to (Type.Size to Type.Size),
        (op.MINUS to Type.Size) to (Type.Size to Type.Size),
        (op.TIMES to Type.Size) to (Type.Size to Type.Size),

        (op.GREATER_THAN_EQUAL to Type.Size) to (Type.Size to Type.Bool),
        (op.LESS_THAN_EQUAL to Type.Size) to (Type.Size to Type.Bool),
        (op.GREATER_THAN to Type.Size) to (Type.Size to Type.Bool),
        (op.LESS_THAN to Type.Size) to (Type.Size to Type.Bool),

        (op.AND to Type.Bool) to (Type.Bool to Type.Bool),
        (op.OR to Type.Bool) to (Type.Bool to Type.Bool),


        (op.EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
        (op.NOT_EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
        (op.EQUALS to Type.Size) to (Type.Size to Type.Bool),
        (op.NOT_EQUALS to Type.Size) to (Type.Size to Type.Bool),
)

data class Discriminant(
    val index: Int,
    val name: Name,
    val params: List<Pair<Name, Type>>
)
