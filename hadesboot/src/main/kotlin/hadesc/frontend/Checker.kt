package hadesc.frontend

import hadesc.Name
import hadesc.analysis.TraitClause
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitResolver
import hadesc.analysis.TypeAnalyzer
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.exhaustive
import hadesc.ir.BinaryOperator
import hadesc.ir.passes.TypeTransformer
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.resolver.Binding
import hadesc.resolver.TypeBinding
import hadesc.types.Substitution
import hadesc.types.Type
import java.util.*
import kotlin.math.min

@OptIn(ExperimentalStdlibApi::class)
class Checker(
        private val ctx: Context
) {
    private var reportErrors = false
    private val typeAnalyzer = TypeAnalyzer()
    private val returnTypeStack = Stack<Type?>()

    fun resolvePropertyBinding(expression: Expression.Property): PropertyBinding? {
        val modulePropertyBinding = ctx.resolver.resolveModuleProperty(expression)
        if (modulePropertyBinding != null) {
            return PropertyBinding.Global(modulePropertyBinding)
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

        error(expression.property, Diagnostic.Kind.NoSuchProperty(
            inferExpression(expression.lhs), expression.property.name))
        return null
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
        val substitution = traitDef.params.zip(traitTypeArgs).map {
            it.first.binder.location to it.second }.toMap()
        val type = Type.Function(
                from = memberSignature.params.map {
                    it.annotation?.let { annot -> annotationToType(annot).applySubstitution(substitution) }
                            ?: Type.Error

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
            if (isExtensionForType(expression.location, extensionDef, inferExpression(expression.lhs))) {
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

            val thisType = instantiateType(expression.location, annotationToType(extensionDef.forType), extensionDef.typeParams)
            val expectedThisType = if (functionDef.signature.thisParamFlags.isPointer) {
                Type.Ptr(
                    thisType,
                    isMutable = functionDef.signature.thisParamFlags.isMutable
                )
            } else {
                thisType
            }
            if (!isTypeAssignableTo(source = inferExpression(expression.lhs), destination = expectedThisType)) {
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
            var methodType: Type = Type.Function(
                from = listOf(receiverType) + functionDef.params.mapIndexed { paramIndex, _ ->
                    typeOfParam(functionDef, paramIndex)
                },
                to = annotationToType(functionDef.signature.returnType),
                traitRequirements = null
            )
            if (extensionDef.typeParams != null) {
                methodType = Type.TypeFunction(
                    extensionDef.typeParams.map { Type.Param(it.binder) },
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
        location: SourceLocation,
        type: Type,
        typeParams: List<TypeParam>?
    ): Pair<Type, Substitution> {
        if (typeParams == null) return type to emptyMap()
        val substitution = instantiateSubstitution(typeParams.map { Type.Param(it.binder) }, location)
        return type.applySubstitution(substitution) to substitution
    }

    private fun isExtensionForType(location: SourceLocation, extensionDef: Declaration.ExtensionDef, type: Type): Boolean {
        val forType = annotationToType(extensionDef.forType)
        return isTypeAssignableTo(
            source = type,
            destination = instantiateType(location, forType, extensionDef.typeParams)
        ) || isTypeAssignableTo(
            source = type,
            destination = Type.Ptr(instantiateType(location, forType, extensionDef.typeParams), isMutable = false))
    }

    private fun instantiateType(location: SourceLocation, type: Type, typeParams: List<TypeParam>?): Type {
        return instantiateTypeAndSubstitution(location, type, typeParams).first
    }


    private fun resolveElementPointerBinding(expression: Expression.Property): PropertyBinding.StructFieldPointer? {
        val lhsType = inferExpression(expression.lhs)
        if (lhsType !is Type.Ptr) {
            return null
        }
        val fieldBinding = resolveStructFieldBinding(lhsType.to, expression.property) ?: return null
        val isMutable = lhsType.isMutable
        return PropertyBinding.StructFieldPointer(
                fieldBinding.structDecl,
                fieldBinding.memberIndex,
                type = Type.Ptr(fieldBinding.type, isMutable)
        )

    }

    public fun resolveStructFieldBinding(lhsType: Type, property: Identifier): PropertyBinding.StructField? {
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
                        ?.map { it.first.binder.location to it.second }
                        ?.toMap()
                        ?: emptyMap()
                val fieldType = annotationToType(field.typeAnnotation).applySubstitution(substitution)
                isTypeAssignableTo(lhsType, fieldType)
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

    private fun equateTypes(source: Type, destination: Type) {
        // isTypeAssignable is side effectful in the sense that it instantiates
        // Type.GenericInstance types for matching source and destination types
        isTypeAssignableTo(source, destination)
    }

    private fun isTypeAssignableTo(source: Type, destination: Type): Boolean {
        return typeAnalyzer.isTypeAssignableTo(source = source, destination = destination)
    }

    private val typeOfExpressionCache = MutableNodeMap<Expression, Type>()
    fun typeOfExpression(expression: Expression): Type {
        val cached = typeOfExpressionCache[expression]
        if (cached != null) {
            return cached
        }
        val def = requireNotNull(ctx.resolver.getEnclosingFunction(expression))
        checkDeclaration(def)
        return requireNotNull(typeOfExpressionCache[expression])

    }

    private val checkedDeclarationSet = MutableNodeMap<Declaration, Unit>()
    fun checkDeclaration(declaration: Declaration) = checkedDeclarationSet.getOrPut(declaration) {
        exhaustive(when (declaration) {
            is Declaration.Error -> {}
            is Declaration.ImportAs -> checkImportAsDeclaration(declaration)
            is Declaration.FunctionDef -> checkFunctionDefDeclaration(declaration)
            is Declaration.ConstDefinition -> checkConstDefinition(declaration)
            is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
            is Declaration.Struct -> checkStructDeclaration(declaration)
            is Declaration.Enum -> TODO()
            is Declaration.TypeAlias -> checkTypeAliasDeclaration(declaration)
            is Declaration.ExtensionDef -> checkExtensionDef(declaration)
            is Declaration.TraitDef -> checkTraitDef(declaration)
            is Declaration.ImplementationDef -> checkImplementationDef(declaration)
        })
    }

    private fun checkImplementationDef(implDef: Declaration.ImplementationDef) {
        implDef.typeParams?.forEach {
            checkTypeParam(it)
        }
        val traitDef = ctx.resolver.resolveDeclaration(implDef.traitRef)
        if (traitDef !is Declaration.TraitDef) {
            error(implDef.traitRef, Diagnostic.Kind.NotATrait)
        } else {
            if (traitDef.params.size > implDef.traitArguments.size) {
                error(implDef.traitRef, Diagnostic.Kind.TooFewTypeArgs)
            }
            if (traitDef.params.size < implDef.traitArguments.size) {
                error(implDef.traitRef, Diagnostic.Kind.TooManyTypeArgs)
            }
        }
        implDef.traitArguments.forEach { annotationToType(it) }
        implDef.whereClause?.let { checkWhereClause(it) }
        for (declaration in implDef.body) {
            checkDeclaration(declaration)
            if (declaration !is Declaration.FunctionDef) {
                error(declaration, Diagnostic.Kind.OnlyFunctionDefsAllowedInsideImplDefs)
            }
        }
    }

    private fun checkWhereClause(whereClause: WhereClause) {
        whereClause.traitRequirements.forEach {
            checkTraitRequirement(it)
        }
    }

    private val checkTraitRequirementCache = MutableNodeMap<TraitRequirementAnnotation, TraitRequirement?>()
    private fun checkTraitRequirement(requirement: TraitRequirementAnnotation): TraitRequirement? = checkTraitRequirementCache.getOrPut(requirement) {
        val declaration = ctx.resolver.resolveDeclaration(requirement.path)
        val args = requirement.typeArgs?.map {
            annotationToType(it)
        }
        if (declaration !is Declaration.TraitDef) {
            error(requirement.path, Diagnostic.Kind.NotATrait)
            null
        } else {
            TraitRequirement(ctx.resolver.qualifiedName(declaration.name), args ?: listOf())
        }

    }

    private fun checkTraitDef(declaration: Declaration.TraitDef) {
        declaration.params.forEach { checkTypeParam(it) }
        if (declaration.params.isEmpty()) {
            error(declaration.name, Diagnostic.Kind.MissingTraitThisParam)
        }
        val methodSet = mutableMapOf<Name, SourceLocation>()
        declaration.signatures.forEach {
            checkFunctionSignature(it)
            val existingBindingLocation = methodSet[it.name.identifier.name]
            if (existingBindingLocation != null) {
                error(it.name, Diagnostic.Kind.DuplicateDeclaration(existingBindingLocation))
            }
            methodSet[it.name.identifier.name] = it.location
            if (it.thisParamFlags != null) {
                error(it.name, Diagnostic.Kind.ReceiverParamsNotAllowedInTraitFunctions)
            }
            it.typeParams?.forEach { typeParam ->
                error(typeParam, Diagnostic.Kind.TypeParamsNotAllowedInTraitFunctions)
            }

        }
    }

    var thisParamType: Type? = null
    private fun checkExtensionDef(declaration: Declaration.ExtensionDef) {
        val forType = annotationToType(declaration.forType)
        for (decl in declaration.declarations) {
            if (decl !is Declaration.FunctionDef) {
                error(decl.startLoc, Diagnostic.Kind.OnlyFunctionDefsAllowedInsideExtensionDefs)
                continue
            }
            val functionDef: Declaration.FunctionDef = decl
            val thisParamFlags = functionDef.signature.thisParamFlags
            require(thisParamType == null)
            if (thisParamFlags != null) {
                thisParamType = if (thisParamFlags.isPointer) {
                    Type.Ptr(
                        forType,
                        isMutable = thisParamFlags.isMutable
                    )
                } else forType
            }
            checkFunctionDefDeclaration(functionDef)
            thisParamType = null
        }
    }

    private fun checkConstDefinition(declaration: Declaration.ConstDefinition) {
        val annotatedType = declaration.annotation?.let { annotationToType(it) }

        val type = if (annotatedType != null) {
            checkExpression(declaration.initializer, annotatedType)
        } else {
            inferExpression(declaration.initializer)
        }
        when (type) {
            is Type.CInt,
            is Type.Bool,
            is Type.Size,
            is Type.Double,
            is Type.Ptr,
            is Type.Integral,
            is Type.FloatingPoint
            -> {}
            else -> {
                error(declaration.name, Diagnostic.Kind.NotAConst)
            }
        }
    }


    private fun checkStructDeclaration(declaration: Declaration.Struct) {
        checkTypeNameBinder(declaration.binder)
        checkValueNameBinder(declaration.binder)
        declaration.typeParams?.forEach {
            checkTypeParam(it)
        }

        fun checkMember(member: Declaration.Struct.Member) = when(member) {
            is Declaration.Struct.Member.Field -> {
                annotationToType(member.typeAnnotation)
            }
        }

        for (member in declaration.members) {
            checkMember(member)
        }
    }

    private fun checkTypeParam(param: TypeParam) {
        // TODO
    }

    private fun checkExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        declaration.paramTypes.forEach { annotationToType(it) }
        annotationToType(declaration.returnType)
    }

    private fun checkImportAsDeclaration(declaration: Declaration.ImportAs) {
        if (ctx.resolveSourceFile(declaration.modulePath) == null) {
            error(declaration.modulePath, Diagnostic.Kind.NoSuchModule)
        }
    }

    private fun checkTypeAliasDeclaration(
        @Suppress("UNUSED_PARAMETER")
        declaration: Declaration.TypeAlias
    ) {
        // TODO
    }

    private fun checkFunctionDefDeclaration(def: Declaration.FunctionDef) {
        checkFunctionSignature(def.signature)
        val returnType = annotationToType(def.signature.returnType)
        returnTypeStack.push(returnType)
        checkBlock(def.body)
        returnTypeStack.pop()

    }

    private fun checkFunctionSignature(signature: FunctionSignature) {
        signature.typeParams?.forEach {
            checkTypeParam(it)
        }
        signature.params.forEach {
            if (it.annotation != null) {
                annotationToType(it.annotation)
            }
        }
        signature.whereClause?.let { checkWhereClause(it) }
        annotationToType(signature.returnType)
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
        is Statement.Return -> checkReturnStatement(statement)
        is Statement.Val -> checkValStatement(statement)
        is Statement.While -> checkWhileStatement(statement)
        is Statement.If -> checkIfStatement(statement)
        is Statement.LocalAssignment -> checkLocalAssignment(statement)
        is Statement.MemberAssignment -> checkMemberAssignment(statement)
        is Statement.PointerAssignment -> checkPointerAssignment(statement)
        is Statement.Defer -> checkDeferStatement(statement)
        is Statement.Error -> {}
    }

    private fun checkDeferStatement(statement: Statement.Defer) {
        checkBlockMember(statement.blockMember)
    }

    private fun checkPointerAssignment(statement: Statement.PointerAssignment) {
        val valueType = inferExpression(statement.lhs)
        val ptrType = inferExpression(statement.lhs.expression)

        if (ptrType !is Type.Ptr) {
            error(statement.lhs, Diagnostic.Kind.NotAPointerType(ptrType))
            inferExpression(statement.value)
            return
        }
        checkExpression(statement.value, valueType)
        if (!ptrType.isMutable) {
            error(statement.lhs, Diagnostic.Kind.ValNotMutable)
        }
    }

    private fun checkLocalAssignment(statement: Statement.LocalAssignment) {
        return when (val binding = ctx.resolver.resolve(statement.name)) {
            is Binding.ValBinding -> {
                if (!binding.statement.isMutable) {
                    error(statement.name, Diagnostic.Kind.AssignmentToImmutableVariable)
                }
                checkValStatement(binding.statement)
                val type = typeOfBinding(binding)
                checkExpression(statement.value, type)
                Unit
            }
            null -> {
                error(statement.name, Diagnostic.Kind.UnboundVariable(statement.name.name))
            }
            else -> {
                // TODO: Show a more helpful diagnostic here
                error(statement.name, Diagnostic.Kind.NotAnAddressableValue)
            }
        }
    }

    private fun checkMemberAssignment(statement: Statement.MemberAssignment) {
        val lhsType = inferExpression(statement.lhs)
        checkExpression(statement.value, lhsType)
        val field = resolvePropertyBinding(statement.lhs)
        if (field !is PropertyBinding.StructField) {
            error(statement.lhs.property, Diagnostic.Kind.NotAStructField)
            return
        }

        if (statement.lhs.lhs !is Expression.Var) {
            error(statement.lhs.lhs, Diagnostic.Kind.NotAnAddressableValue)
        } else {
            val binding = ctx.resolver.resolve(statement.lhs.lhs.name)
            if (binding == null) {
                error(statement.lhs.lhs, Diagnostic.Kind.UnboundVariable(statement.lhs.lhs.name.name))
                return
            }
            if (binding !is Binding.ValBinding) {
                error(statement.lhs, Diagnostic.Kind.NotAnAddressableValue)
                return
            }
            if (!binding.statement.isMutable) {
                error(statement.lhs.lhs, Diagnostic.Kind.ValNotMutable)
            }
        }
    }

    private fun checkAssignability(node: HasLocation, source: Type, destination: Type) {
        if (!isTypeAssignableTo(destination = destination, source = source)) {
            error(node, Diagnostic.Kind.TypeNotAssignable(
                source = reduceGenericInstances(source),
                destination = reduceGenericInstances(destination)
            ))
        }
    }

    private fun checkWhileStatement(statement: Statement.While) {
        checkExpression(statement.condition, Type.Bool)
        checkBlock(statement.body)
    }

    private fun checkIfStatement(statement: Statement.If) {
        checkExpression(statement.condition, Type.Bool)
        checkBlock(statement.ifTrue)
        statement.ifFalse?.let { checkBlock(it) }
    }

    private fun checkReturnStatement(statement: Statement.Return) {
        if (statement.value != null) {
            val returnType = returnTypeStack.peek()
            if (returnType != null) {
                checkExpression(statement.value, returnType)
            } else {
                inferExpression(statement.value)
            }
        }
    }

    private fun checkValStatement(statement: Statement.Val) {
        val expectedType = statement.typeAnnotation?.let { annotationToType(it) }
        if (expectedType != null) {
            checkExpression(statement.rhs, expectedType)
        } else {
            inferExpression(statement.rhs)
        }
        checkValueNameBinder(statement.binder)
    }

    private fun checkTypeNameBinder(binder: Binder) {
        // TODO
    }

    private fun checkValueNameBinder(
        @Suppress("UNUSED_PARAMETER")
        name: Binder) {
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
                if (isEqualityCheckingOperator(expression.operator)) {
                    if (!isTypeEqualityComparable(lhsType)) {
                        error(expression, Diagnostic.Kind.TypeNotEqualityComparable(lhsType))
                    }
                } else if (isOrderPredicateOperator(expression.operator)) {
                    if (!isTypeOrderComparable(lhsType)) {
                        error(expression, Diagnostic.Kind.OperatorNotApplicable(expression.operator))
                    }
                }
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
                else -> {
                    val inferredType = inferExpression(expression)
                    checkAssignability(expression, source = inferredType, destination = expectedType)
                    expectedType
                }
            }
        }
        typeOfExpressionCache[expression] = type
        return type
    }

    private val closureParamTypes = mutableMapOf<Binder, Type>()
    private fun checkOrInferClosureExpression(expression: Expression.Closure, expectedType: Type?): Type {
        val functionTypeComponents = expectedType?.let { getFunctionTypeComponents(it) }
        val expectedReturnType = functionTypeComponents?.to ?: expression.returnType?.let { annotationToType(it) }
        if (functionTypeComponents != null && expression.returnType != null) {
            checkAssignability(
                expression.returnType,
                source = annotationToType(expression.returnType), destination = functionTypeComponents.to)
        }
        returnTypeStack.push(expectedReturnType)
        val expectedParamTypes = expectedType?.let { getFunctionTypeComponents(it) }
        var index = -1
        val paramTypes = mutableListOf<Type>()
        for (param in expression.params) {
            index++
            val expectedParamType = expectedParamTypes?.from?.getOrNull(index)
            val type = if (param.annotation != null) {
                val annotatedType = annotationToType(param.annotation)
                if (expectedParamType != null) {
                    checkAssignability(
                        node = param.annotation,
                        // params are contravariant
                        destination = annotatedType,
                        source = expectedParamType
                    )
                }
                annotatedType
            } else if (expectedParamType != null) {
                expectedParamType
            } else {
                error(param, Diagnostic.Kind.MissingTypeAnnotation)
                Type.Error
            }
            closureParamTypes[param.binder] = type
            paramTypes.add(type)
        }

        val returnType = when (expression.body) {
            is ClosureBody.Block -> {
                if (expectedReturnType == null) {
                    // TODO: We can infer block return types using the return statement
                    //       inside the closure if it exists. For now, we can just show an error
                    error(expression.body.block.startToken, Diagnostic.Kind.ReturnTypeNotInferred)
                }
                checkBlock(expression.body.block)
                expectedReturnType ?: Type.Error
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

    private fun isTypeOrderComparable(type: Type): Boolean {
        return type is Type.Integral || type is Type.CInt || type is Type.FloatingPoint || type is Type.Size
    }

    private fun isTypeEqualityComparable(type: Type): Boolean {
        return type is Type.Integral || type is Type.CInt || type is Type.Bool ||
                type is Type.Ptr || type is Type.Size || type is Type.Byte
    }

    private fun checkNullPtrExpression(expression: Expression.NullPtr, expectedType: Type): Type {
        if (expectedType !is Type.Ptr) {
            error(expression, Diagnostic.Kind.NotAPointerType(expectedType))
        }
        return expectedType
    }

    private fun isIntLiteralAssignable(type: Type): Boolean = when(type) {
        is Type.Size,
        is Type.CInt -> true
        is Type.Integral -> true
        is Type.Byte -> true
        else -> false
    }

    private fun inferExpression(expression: Expression): Type = typeOfExpressionCache.getOrPut(expression) {
        reduceGenericInstances(when (expression) {
            is Expression.Error -> Type.Error
            is Expression.Var -> inferVarExpresion(expression)
            is Expression.Call -> inferCallExpression(expression)
            is Expression.Property -> inferPropertyExpression(expression)
            is Expression.ByteString -> Type.Ptr(Type.Byte, isMutable = false)
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
            is Expression.TypeApplication -> TODO()
            is Expression.Match -> TODO()
            is Expression.New -> inferNewExpression(expression)
            is Expression.PipelineOperator -> inferPipelineOperator(expression)
            is Expression.This -> inferThisExpression(expression)
            is Expression.Closure -> checkOrInferClosureExpression(expression, expectedType = null)
            is Expression.TraitMethodCall -> inferTraitMethodCall(expression)
        })
    }

    private fun inferTraitMethodCall(expression: Expression.TraitMethodCall): Type {
        return inferOrCheckCallLikeExpression(
                callNode = expression,
                typeArgs = null,
                args = expression.args,
                expectedReturnType = null
        )
    }

    private fun inferTypeApplication(expression: Expression.TypeApplication): Type {
        TODO()
    }

    private fun inferThisExpression(expression: Expression.This): Type {
        val thisType = thisParamType
        if (thisType == null) {
            error(expression, Diagnostic.Kind.UnboundThis)
            return Type.Error
        }
        return thisType
    }

    private fun inferPipelineOperator(expression: Expression.PipelineOperator): Type {
        return inferOrCheckCallLikeExpression(
            callNode = expression,
            typeArgs = null,
            args = listOf(Arg(expression.lhs)),
            expectedReturnType = null
        )
    }

    private fun inferNewExpression(expression: Expression.New): Type {
        return Type.Ptr(inferOrCheckCallLikeExpression(
            callNode = expression,
            args = expression.args,
            typeArgs = expression.typeArgs,
            expectedReturnType = null
        ), isMutable = true)
    }

    private fun checkConstructorFunction(path: QualifiedPath): Type {
        return when (val declaration = ctx.resolver.resolveDeclaration(path)) {
            null -> {
                error(path, Diagnostic.Kind.UnboundTypePath(path))
                Type.Error
            }
            is Declaration.Struct -> {
                typeOfBinder(declaration.binder)
            }
            else -> {
                error(path, Diagnostic.Kind.NotAConstructor)
                Type.Error
            }
        }
    }

    private fun inferDerefExpression(expression: Expression.Deref): Type {
        val ptrType = inferExpression(expression.expression)
        return if (ptrType !is Type.Ptr) {
            error(expression, Diagnostic.Kind.NotAPointerType(ptrType))
            Type.Error
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
            if ((expression.operator == BinaryOperator.EQUALS || expression.operator == BinaryOperator.NOT_EQUALS) &&  lhsType is Type.Ptr) {
                checkExpression(expression.rhs, lhsType.copy(isMutable = false))
                Type.Bool
            } else {
                inferExpression(expression.rhs)
                error(expression.location, Diagnostic.Kind.OperatorNotApplicable(expression.operator))
                Type.Error
            }
        }
    }

    private fun inferPointerCast(expression: Expression.PointerCast): Type {
        val targetPointerToType = annotationToType(expression.toType)
        val argType = inferExpression(expression.arg)
        return if (argType !is Type.Ptr) {
            error(expression.toType, Diagnostic.Kind.NotAPointerType(argType))
            Type.Ptr(targetPointerToType, isMutable = true)
        } else {
            Type.Ptr(targetPointerToType, isMutable = argType.isMutable)
        }
    }

    private fun inferNullPtrExpression(expression: Expression.NullPtr): Type {
        error(expression, Diagnostic.Kind.MissingTypeAnnotation)
        return Type.Error
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
        return Type.CInt
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
                null -> Type.Error
            }

    private fun typeOfGlobalPropertyBinding(
            binding: PropertyBinding.Global): Type {
        return typeOfBinding(binding.binding)
    }

    private fun inferVarExpresion(expression: Expression.Var): Type {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> {
                error(expression.name, Diagnostic.Kind.UnboundVariable(expression.name.name))
                Type.Error
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
        is Binding.EnumCaseConstructor -> TODO()
        is Binding.Pattern -> TODO()
        is Binding.WhereParam -> typeOfWhereParamBinding(binding)
        is Binding.ClosureParam -> typeOfClosureParam(binding)
    }

    private fun typeOfWhereParamBinding(binding: Binding.WhereParam): Type {
        TODO()
//        return binding.param.annotation?.let { annotationToType(it) } ?: Type.Error
    }

    private fun typeOfClosureParam(binding: Binding.ClosureParam): Type {
        return closureParamTypes[binding.param.binder] ?: Type.Error
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
        val functionType = Type.Function(from = fieldTypes, to = instanceType, traitRequirements = null)
        val type = if (binding.declaration.typeParams == null) {
            functionType
        } else {
            Type.TypeFunction(
                    params = binding.declaration.typeParams.map { Type.Param(it.binder) },
                    body = functionType
            )
        }
        return Type.Ptr(
                type,
                isMutable = false
        )
    }

    private fun structFieldTypes(declaration: Declaration.Struct): Map<Name, Type> {
        return declaration.members.map {
            when (it) {
                is Declaration.Struct.Member.Field ->
                    it.binder.identifier.name to annotationToType(it.typeAnnotation)
            }
        }.toMap()
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
            Type.Error
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
                    param.annotation?.let { annotationToType(it) } ?: Type.Error },
                to = annotationToType(declaration.signature.returnType),
                traitRequirements = declaration.signature.whereClause
                        ?.traitRequirements?.mapNotNull { checkTraitRequirement(it) }
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
            val typeParams: List<Type.Param>?,
            val traitRequirements: List<TraitRequirement>?
    )

    private fun inferCallExpression(expression: Expression.Call): Type {
        return inferOrCheckCallLikeExpression(
                callNode = expression,
                typeArgs = expression.typeArgs,
                args = expression.args,
                expectedReturnType = null
        )
    }
    private fun checkCallExpression(expression: Expression.Call, expectedType: Type): Type {
        return inferOrCheckCallLikeExpression(
                callNode = expression,
                typeArgs = expression.typeArgs,
                args = expression.args,
                expectedReturnType = expectedType
        )
    }

    private fun inferOrCheckCallLikeExpression(
            callNode: Expression,
            typeArgs: List<TypeAnnotation>?,
            args: List<Arg>,
            expectedReturnType: Type?
    ): Type {
        val explicitTypeArgs = typeArgs?.map { annotationToType(it) }
        val calleeType = getCalleeType(callNode)
        val functionType = getFunctionTypeComponents(calleeType)
        return if (functionType == null) {
            for (arg in args) {
                inferExpression(arg.expression)
            }
            error(callNode, Diagnostic.Kind.TypeNotCallable(calleeType))
            Type.Error
        } else {
            val substitution = instantiateSubstitution(
                functionType.typeParams,
                callNode.location
            )
            val receiver = getCallReceiver(callNode)
            val argsWithReceiver = if (receiver == null)
                args.map { it.expression }
            else listOf(receiver) + args.map { it.expression }
            checkCallArgs(functionType, callNode, explicitTypeArgs, argsWithReceiver, substitution)
            if (expectedReturnType != null) {
                val source = functionType.to.applySubstitution(substitution)
                checkAssignability(
                        callNode,
                        source = source,
                        destination = expectedReturnType
                )
            }
            if (functionType.traitRequirements != null) {
                checkTraitInstances(
                        callNode,
                        functionType.traitRequirements.map { it.applySubstitution(substitution) }
                )
            }
            if (functionType.typeParams != null) {
                getTypeArgsCache[callNode] = functionType.typeParams.map {
                    requireNotNull(substitution[it.binder.location])
                }
            }

            reduceGenericInstances(functionType.to.applySubstitution(substitution))
        }

    }

    private fun checkTraitInstances(callNode: Expression, requiredInstances: List<TraitRequirement>) {
        for (requirement in requiredInstances) {
            if (!isRequirementSatisfied(callNode, requirement)) {
                error(callNode, Diagnostic.Kind.NoImplementationFound(
                        TraitRequirement(
                                requirement.traitRef,
                                requirement.arguments.map { reduceGenericInstances(it) })
                ))
            }
        }
    }

    private val globalTraitClauses by lazy {
        ctx.resolver.implementationDefs.mapNotNull { implementationDef ->
            val traitDef = ctx.resolver.resolveDeclaration(implementationDef.traitRef)
            if (traitDef !is Declaration.TraitDef) {
                null
            } else {
                TraitClause.Implementation(
                        params = implementationDef.typeParams?.map { p -> Type.Param(p.binder) } ?: emptyList(),
                        traitRef = ctx.resolver.qualifiedName(traitDef.name),
                        arguments = implementationDef.traitArguments.map { annotationToType(it) },
                        requirements = implementationDef.whereClause?.traitRequirements?.mapNotNull { checkTraitRequirement(it) } ?: emptyList()
                )
            }
        }
    }

    private fun isRequirementSatisfied(callNode: Expression, requiredInstance: TraitRequirement): Boolean {
        val clauses = globalTraitClauses
        val enclosingFunction = requireNotNull(ctx.resolver.getEnclosingFunction(callNode))
        val functionTraitClauses = enclosingFunction.traitRequirements.map { it.toClause() }
        val enclosingImpl = ctx.resolver.getEnclosingImpl(callNode)
        val implTraitClauses = enclosingImpl?.traitRequirements?.map { it.toClause() } ?: emptyList()
        val env = TraitResolver.Env(functionTraitClauses + implTraitClauses + clauses)
        val traitResolver = TraitResolver(env, typeAnalyzer)
        return traitResolver.isTraitImplemented(requiredInstance.traitRef, requiredInstance.arguments)
    }

    private fun TraitRequirement.toClause(): TraitClause {
        return TraitClause.Requirement(this)
    }

    private val Declaration.FunctionDef.traitRequirements get(): List<TraitRequirement> =
        signature.whereClause
            ?.traitRequirements?.mapNotNull { checkTraitRequirement(it) }
            ?: emptyList()

    private val Declaration.ImplementationDef.traitRequirements get(): List<TraitRequirement> =
        whereClause
                ?.traitRequirements
                ?.mapNotNull { checkTraitRequirement(it) }
                ?: emptyList()


    private fun getCallReceiver(callNode: Expression): Expression? {
        if (callNode !is Expression.Call) {
            return null
        }
        if (callNode.callee !is Expression.Property) {
            return null
        }
        if (resolvePropertyBinding(callNode.callee) !is PropertyBinding.ExtensionDef) {
            return null
        }
        return callNode.callee.lhs
    }

    private fun getCalleeType(callNode: Expression): Type = when(callNode) {
        is Expression.Call -> when (callNode.callee) {
            is Expression.Property -> when (resolvePropertyBinding(callNode.callee)) {
                else -> inferExpression(callNode.callee)
            }
            else -> inferExpression(callNode.callee)
        }
        is Expression.TraitMethodCall -> {
            when (val traitDeclaration = ctx.resolver.resolveDeclaration(callNode.traitName)) {
                !is Declaration.TraitDef -> Type.Error
                else -> {
                    val methodSignature = traitDeclaration.findMethodSignature(callNode.methodName)
                    if (methodSignature == null) {
                        Type.Error
                    } else {
                        val substitution = traitDeclaration.params.zip(callNode.traitArgs).map {
                            it.first.binder.location to annotationToType(it.second)
                        }.toMap()
                        Type.Function(
                                from = methodSignature.params.map {
                                    if (it.annotation == null) Type.Error
                                    else annotationToType(it.annotation).applySubstitution(substitution)
                                },
                                to = annotationToType(methodSignature.returnType).applySubstitution(substitution),
                                traitRequirements = null
                        )
                    }
                }
            }
        }
        is Expression.New -> checkConstructorFunction(callNode.qualifiedPath)
        is Expression.PipelineOperator -> inferExpression(callNode.rhs)
        else -> Type.Error
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
        instantiationLocation: SourceLocation
    ): Substitution {
        return typeParams
                ?.map { it.binder.location to makeGenericInstance(
                    it.binder
                ) }
                ?.toMap()
                ?: emptyMap()
    }

    private fun checkCallArgs(
            functionType: FunctionTypeComponents,
            callNode: HasLocation,
            typeArgs: List<Type>?,
            args: List<Expression>,
            substitution: Substitution
    ) {
        val length = min(functionType.from.size, args.size)
        typeArgs?.zip(functionType.typeParams ?: emptyList())?.forEach { (arg, param) ->
            val expectedType = Type.ParamRef(param.binder).applySubstitution(substitution)
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

        checkArgumentLength(callNode, args, functionType)
    }

    private fun checkArgumentLength(
            callNode: HasLocation,
            args: List<Expression>,
            functionType: FunctionTypeComponents
    ) {
        if (args.size < functionType.from.size) {
            error(callNode, Diagnostic.Kind.MissingArgs(required = functionType.from.size))
        }

        if (args.size > functionType.from.size) {
            error(callNode, Diagnostic.Kind.TooManyArgs(required = functionType.from.size))
        }
    }

    private fun getFunctionTypeComponents(type: Type): FunctionTypeComponents? {
        fun recurse(type: Type, typeParams: List<Type.Param>?): FunctionTypeComponents? {
            return when (type) {
                is Type.Ptr -> recurse(type.to, null)
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
                is Type.GenericInstance -> recurse(reduceGenericInstances(type), null)
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
    fun getTypeArgs(expression: Expression): List<Type>? =
        getTypeArgsCache[expression]?.map { reduceGenericInstances(it) }


    private val annotationToTypeCache = MutableNodeMap<TypeAnnotation, Type>()
    fun annotationToType(annotation: TypeAnnotation): Type = annotationToTypeCache.getOrPut(annotation) {
        when (annotation) {
            is TypeAnnotation.Error -> Type.Error
            is TypeAnnotation.Var -> varAnnotationToType(annotation)
            is TypeAnnotation.Ptr -> ptrAnnotationToType(annotation)
            is TypeAnnotation.MutPtr -> mutPtrAnnotationToType(annotation)
            is TypeAnnotation.Application -> typeApplicationAnnotationToType(annotation)
            is TypeAnnotation.Qualified -> qualifiedAnnotationToType(annotation)
            is TypeAnnotation.Function -> functionAnnotationToType(annotation)
            is TypeAnnotation.Union -> unionAnnotationToType(annotation)
        }
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
            val substitution = callee.params.zip(args).map {
                it.first.binder.location to it.second
            }.toMap()
            callee.body.applySubstitution(substitution)
        } else {
            Type.Error
        }
    }

    private fun qualifiedAnnotationToType(annotation: TypeAnnotation.Qualified): Type {
        val binding = ctx.resolver.resolveQualifiedType(annotation.qualifiedPath)
        return if (binding == null) {
            error(annotation, Diagnostic.Kind.UnboundTypePath(annotation.qualifiedPath))
            Type.Error
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

    private fun varAnnotationToType(annotation: TypeAnnotation.Var): Type {
        val resolved = resolveTypeVariable(annotation)
        if (resolved == null) {
            error(annotation, Diagnostic.Kind.UnboundType(annotation.name.name))
        }
        return resolved ?: Type.Error
    }

    private fun resolveTypeVariable(annotation: TypeAnnotation.Var): Type? {
        val binding = ctx.resolver.resolveTypeVariable(annotation.name)
        return if (binding == null) {
            when (annotation.name.name.text) {
                "Int" -> Type.CInt
                "Bool" -> Type.Bool
                "Byte" -> Type.Byte
                "Size" -> Type.Size
                "Double" -> Type.Double
                "Void" -> Type.Void
                "u8" -> Type.Integral(8, false)
                "i8" -> Type.Integral(8, true)
                "u16" -> Type.Integral(16, false)
                "i16" -> Type.Integral(16, true)
                "u32" -> Type.Integral(32, false)
                "i32" -> Type.Integral(32, true)
                "u64" -> Type.Integral(64, false)
                "i64" -> Type.Integral(64, true)
                "f16" -> Type.FloatingPoint(16)
                "f32" -> Type.FloatingPoint(32)
                "f64" -> Type.FloatingPoint(64)
                else -> null
            }
        } else {
            typeOfTypeBinding(binding)
        }
    }

    private fun typeOfTypeBinding(binding: TypeBinding): Type {
        return when (binding) {
            is TypeBinding.Struct -> typeOfStructBinding(binding)
            is TypeBinding.TypeParam -> typeOfTypeParam(binding)
            is TypeBinding.Enum -> TODO()
            is TypeBinding.TypeAlias -> typeOfTypeAlias(binding)
            is TypeBinding.Trait -> typeOfTraitTypeBinding(binding)
        }
    }

    private fun typeOfTypeParam(binding: TypeBinding.TypeParam): Type {
        return Type.ParamRef(binding.binder)
    }

    fun typeOfBinder(binder: Binder): Type = when (val binding = ctx.resolver.resolve(binder.identifier)) {
        null -> Type.Error
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
                    val substitution = type.callee.params.zip(type.args)
                            .map { it.first.binder.location to it.second }
                            .toMap()
                    return type.callee.body.applySubstitution(substitution)
                } else {
                    super.lowerTypeApplication(type)
                }
            }
        }.lowerType(type)
    }

    private fun error(node: HasLocation, kind: Diagnostic.Kind) {
        if (reportErrors) {
            ctx.diagnosticReporter.report(node.location, kind)
        }
    }

    fun enableDiagnostics() {
        reportErrors = true
    }

    fun disableDiagnostics() {
        reportErrors = false
    }

}

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
        (op.OR to Type.Bool) to (Type.Bool to Type.Bool),


        (op.EQUALS to Type.CInt) to (Type.CInt to Type.Bool),
        (op.NOT_EQUALS to Type.CInt) to (Type.CInt to Type.Bool),
        (op.EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
        (op.NOT_EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
        (op.EQUALS to Type.Size) to (Type.Size to Type.Bool),
        (op.NOT_EQUALS to Type.Size) to (Type.Size to Type.Bool),

        (op.EQUALS to Type.Byte) to (Type.Byte to Type.Bool),
        (op.NOT_EQUALS to Type.Byte) to (Type.Byte to Type.Bool),
)

