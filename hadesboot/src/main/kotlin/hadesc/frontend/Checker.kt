package hadesc.frontend

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourcePath
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.unit
import libhades.collections.Stack

class Checker(val ctx: Context) {
    private val returnTypeStack = Stack<Type>()
    fun checkProgram() {
        ctx.forEachSourceFile { sourceFile ->
            checkSourceFile(sourceFile)
        }
    }

    private fun checkSourceFile(sourceFile: SourceFile) {
        sourceFile.declarations.forEach { declaration ->
            checkDeclaration(declaration)
        }
    }

    private fun checkDeclaration(declaration: Declaration) = when(declaration) {
        is Declaration.Error -> {}
        is Declaration.ImportAs -> checkImportAsDeclaration(declaration)
        is Declaration.ImportMembers -> checkImportMembersDeclaration(declaration)
        is Declaration.FunctionDef -> checkFunctionDef(declaration)
        is Declaration.ConstDefinition -> checkConstDefinition(declaration)
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.Struct -> checkStructDeclaration(declaration)
        is Declaration.TypeAlias -> checkTypeAlias(declaration)
        is Declaration.ExtensionDef -> checkExtensionDef(declaration)
        is Declaration.TraitDef -> checkTraitDef(declaration)
        is Declaration.ImplementationDef -> checkImplementationDef(declaration)
        is Declaration.SealedType -> checkSealedTypeDef(declaration)
    }

    private fun checkSealedTypeDef(declaration: Declaration.SealedType) {
        declaration.typeParams?.let { checkTypeParams(it) }
        checkTopLevelTypeBinding(declaration.name)
        val caseNames = mutableSetOf<Name>()
        declaration.cases.forEach { case ->
            if (case.name.name in caseNames) {
                error(case.name, Diagnostic.Kind.DuplicateVariantName)
            } else {
                caseNames.add(case.name.name)
            }

            case.params?.forEach { param ->
                if (param.annotation == null) {
                    error(param, Diagnostic.Kind.MissingTypeAnnotation)
                } else {
                    checkTypeAnnotation(param.annotation)
                }
            }
        }
    }

    private fun checkImplementationDef(implDef: Declaration.ImplementationDef) {
        implDef.typeParams?.let { checkTypeParams(it) }
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
        implDef.traitArguments.forEach { it.type }
        implDef.whereClause?.let { checkWhereClause(it) }

        val expectedMethods = if (traitDef is Declaration.TraitDef) {
            traitDef.expectedMethods(implDef.traitArguments.map { it.type })
        } else emptyMap()
        val foundMethods = mutableSetOf<Name>()
        for (declaration in implDef.body) {
            checkDeclaration(declaration)
            if (declaration !is Declaration.FunctionDef) {
                error(declaration, Diagnostic.Kind.OnlyFunctionDefsAllowedInsideImplDefs)
            } else {
                foundMethods.add(declaration.name.identifier.name)
                val typeOfMethod = ctx.analyzer.typeOfBinder(declaration.name)
                require(typeOfMethod is Type.Ptr)
                val actualType = typeOfMethod.to
                val expectedType = expectedMethods[declaration.name.identifier.name]
                if (expectedType != null && !actualType.isAssignableTo(expectedType)) {
                    error(declaration.name, Diagnostic.Kind.TraitMethodTypeMismatch(expected = expectedType, found = actualType))
                }
            }
        }

        for (name in expectedMethods.keys) {
            if (name !in foundMethods) {
                error(implDef.traitRef, Diagnostic.Kind.MissingImplMethod(name))
            }
        }
    }
    private fun Declaration.TraitDef.expectedMethods(typeArguments: List<Type>): Map<Name, Type> {
        val map = mutableMapOf<Name, Type>()
        val substitution = params.zip(typeArguments)
            .map { it.first.binder.location to it.second }
            .toMap()
        for (method in signatures) {
            val paramTypes = method.params
                .map { it.annotation }
                .map { it?.type ?: Type.Error }
                .map { it.applySubstitution(substitution) }
            val returnType = method.returnType.type.applySubstitution(substitution)
            val fnType = Type.Function(
                from = paramTypes,
                to = returnType,
                traitRequirements = null
            )
            map[method.name.identifier.name] = fnType
        }
        return map

    }

    private fun checkWhereClause(whereClause: WhereClause) {
        whereClause.traitRequirements.forEach {
            checkTraitRequirement(it)
        }
    }

    private fun checkTraitRequirement(requirement: TraitRequirementAnnotation) {
        val declaration = ctx.resolver.resolveDeclaration(requirement.path)
        requirement.typeArgs?.forEach {
            checkTypeAnnotation(it)
        }
        if (declaration !is Declaration.TraitDef) {
            error(requirement.path, Diagnostic.Kind.NotATrait)
        }
    }


    private val TypeAnnotation.type get() = ctx.analyzer.annotationToType(this)

    private fun checkTraitDef(declaration: Declaration.TraitDef) {
        checkTopLevelTypeBinding(declaration.name)
        checkTypeParams(declaration.params)
        if (declaration.params.isEmpty()) {
            error(declaration.name, Diagnostic.Kind.MissingTraitThisParam)
        }
        declaration.signatures.forEach { checkFunctionSignature(it, skipThisParamCheck = true) }
        declaration.signatures.forEach { signature ->
            if (signature.thisParamFlags != null) {
                error(signature.name, Diagnostic.Kind.ReceiverParamsNotAllowedInTraitFunctions)
            }

            if (signature.typeParams != null) {
                error(signature.name, Diagnostic.Kind.TypeParamsNotAllowedInTraitFunctions)
            }
        }
        val names = mutableMapOf<Name, Binder>()
        declaration.signatures.forEach { signature ->
            val declared = names[signature.name.name]
            if (declared != null) {
                error(signature.name.identifier, Diagnostic.Kind.DuplicateDeclaration(declared.location))
            } else {
                names[signature.name.name] = signature.name
            }
        }
    }

    private fun checkConstDefinition(declaration: Declaration.ConstDefinition) {
        declaration.annotation?.let { checkTypeAnnotation(it) }
        val annotatedType = declaration.annotation?.let { ctx.analyzer.annotationToType(it) }
        checkExpression(declaration.initializer)
        if (annotatedType != null) {
            checkExpressionHasType(declaration.initializer, annotatedType)
        }
        if (!ctx.analyzer.isCompileTimeConstant(declaration.initializer)) {
            error(declaration.initializer, Diagnostic.Kind.NotAConst)
        }
    }

    private fun checkExtensionDef(declaration: Declaration.ExtensionDef) {
        checkTypeAnnotation(declaration.forType)
        for (decl in declaration.declarations) {
            if (decl !is Declaration.FunctionDef) {
                error(decl.startLoc, Diagnostic.Kind.OnlyFunctionDefsAllowedInsideExtensionDefs)
                continue
            }
            val functionDef: Declaration.FunctionDef = decl
            checkDeclaration(functionDef)
        }
    }

    private fun checkStructDeclaration(declaration: Declaration.Struct) {
        checkTopLevelTypeBinding(declaration.binder)
        declaration.typeParams?.let { checkTypeParams(it) }
        val declaredFields = mutableMapOf<Name, Declaration.Struct.Member.Field>()
        for (member in declaration.members) {
            when (member) {
                is Declaration.Struct.Member.Field -> {
                    val existingField = declaredFields[member.binder.name]
                    if (existingField != null) {
                        error(member.binder, Diagnostic.Kind.DuplicateValueBinding(existingField.binder))
                    } else {
                        declaredFields[member.binder.name] = member
                    }
                    checkTypeAnnotation(member.typeAnnotation)
                }
            }
        }
    }

    private fun checkExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        checkTopLevelExpressionBinding(declaration.binder)
        declaration.paramTypes.forEach { checkTypeAnnotation(it) }
        checkTypeAnnotation(declaration.returnType)
    }

    private fun checkTypeAlias(declaration: Declaration.TypeAlias) {
        checkTopLevelTypeBinding(declaration.name)
        declaration.typeParams?.let {
            checkTypeParams(it)
        }
        checkTypeAnnotation(declaration.rhs)
    }

    private fun checkTypeAnnotation(annotation: TypeAnnotation): Unit = when(annotation) {
        is TypeAnnotation.Error -> unit
        is TypeAnnotation.Var -> {
            val resolved = ctx.resolver.resolveTypeVariable(annotation.name)
            if (resolved == null) {
                error(annotation, Diagnostic.Kind.UnboundType(annotation.name.name))
            }
            unit
        }
        is TypeAnnotation.Ptr -> {
            checkTypeAnnotation(annotation.to)
        }
        is TypeAnnotation.MutPtr -> {
            checkTypeAnnotation(annotation.to)
        }
        is TypeAnnotation.Application -> checkTypeApplicationAnnotation(annotation)
        is TypeAnnotation.Qualified -> {
            val binding = ctx.resolver.resolveQualifiedType(annotation.qualifiedPath)
            if (binding == null) {
                error(annotation, Diagnostic.Kind.UnboundTypePath(annotation.qualifiedPath))
            }
            unit
        }
        is TypeAnnotation.Function -> {
            annotation.from.map {
                checkTypeAnnotation(it)
            }
            checkTypeAnnotation(annotation.to)
        }
        is TypeAnnotation.Union -> {
            annotation.args.forEach {
                checkTypeAnnotation(it)
            }
        }
    }

    private fun checkTypeApplicationAnnotation(annotation: TypeAnnotation.Application) {
        checkTypeAnnotation(annotation.callee)
        annotation.args.map { checkTypeAnnotation(it) }

        val calleeType = ctx.analyzer.annotationToType(annotation.callee)
        val args = annotation.args.map { ctx.analyzer.annotationToType(it) }
        if (calleeType !is Type.TypeFunction) {
            error(annotation.callee, Diagnostic.Kind.InvalidTypeApplication)
        } else {
            if (calleeType.params.size != args.size) {
                error(annotation.callee, Diagnostic.Kind.InvalidTypeApplication)
            }
        }
    }

    private fun checkTypeParams(params: List<TypeParam>) {
        val binders = mutableMapOf<Name, Binder>()
        params.forEach { param ->
            val existing = binders[param.binder.name]
            if (existing != null) {
                error(param, Diagnostic.Kind.DuplicateTypeBinding(existing))
            } else {
                binders[param.binder.name] = param.binder
            }
        }
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        checkFunctionSignature(declaration.signature)
        returnTypeStack.push(ctx.analyzer.annotationToType(declaration.signature.returnType))
        checkBlock(declaration.body)
        returnTypeStack.pop()

    }

    private fun checkBlock(body: Block) {
        body.members.forEach { checkBlockMember(it) }
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
        is Statement.Error -> Unit
    }

    private fun checkMemberAssignment(statement: Statement.MemberAssignment) {
        checkExpression(statement.lhs)
        checkExpression(statement.value)

        checkExpressionHasType(statement.value, statement.lhs.type)

        val field = ctx.analyzer.resolvePropertyBinding(statement.lhs)
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

    private fun checkPointerAssignment(statement: Statement.PointerAssignment) {
        checkExpression(statement.lhs)
        checkExpression(statement.value)

        val lhsType = statement.lhs.expression.type
        if (lhsType !is Type.Ptr) {
            error(statement.lhs, Diagnostic.Kind.NotAPointerType(lhsType))
            return
        }

        if (!lhsType.isMutable) {
            error(statement.lhs, Diagnostic.Kind.ValNotMutable)
        }
        checkExpressionHasType(statement.value, lhsType.to)
    }

    private fun checkLocalAssignment(statement: Statement.LocalAssignment) {
        checkExpression(statement.value)
        val binding = ctx.resolver.resolve(statement.name)
        if (binding !is Binding.ValBinding || !binding.statement.isMutable) {
            error(statement.name, Diagnostic.Kind.ValNotMutable)
        }
        if (binding is Binding.ValBinding) {
            val expectedType = binding.statement.typeAnnotation?.let { ctx.analyzer.annotationToType(it) }
                ?: ctx.analyzer.typeOfExpression(binding.statement.rhs)
            checkExpressionHasType(statement.value, expectedType)
        }
    }

    private fun checkWhileStatement(statement: Statement.While) {
        checkExpressionHasType(statement.condition, Type.Bool)
        checkBlock(statement.body)
    }

    private fun checkIfStatement(statement: Statement.If) {
        checkExpressionHasType(statement.condition, Type.Bool)
        checkBlock(statement.ifTrue)
        statement.ifFalse?.let { checkBlock(it) }
    }

    private fun checkExpressionHasType(expression: Expression, type: Type) {
        val exprType = ctx.analyzer.typeOfExpression(expression)
        if (!exprType.isAssignableTo(type)) {
            error(expression, Diagnostic.Kind.TypeNotAssignable(source = exprType, destination = type))
        }
    }

    private fun checkDeferStatement(statement: Statement.Defer) {
        checkBlockMember(statement.blockMember)
    }

    private fun checkBlockMember(blockMember: Block.Member) = when(blockMember) {
        is Block.Member.Expression -> checkExpression(blockMember.expression)
        is Block.Member.Statement -> checkStatement(blockMember.statement)
    }

    private fun checkReturnStatement(statement: Statement.Return) {
        if (statement.value != null) {
            checkExpression(statement.value)
        }
        val expectedReturnType = requireNotNull(returnTypeStack.peek())

        if (expectedReturnType.isAssignableTo(Type.Void)
            && statement.value != null) {
            error(statement, Diagnostic.Kind.ReturningFromVoidFunction)
        }
        if (statement.value != null) {
            checkExpressionHasType(statement.value, expectedReturnType)
        }
    }

    private fun checkValStatement(statement: Statement.Val) {
        statement.typeAnnotation?.let {
            checkTypeAnnotation(it)
        }
        checkExpression(statement.rhs)

        if (statement.typeAnnotation != null) {
            val annotatedType = ctx.analyzer.annotationToType(statement.typeAnnotation)
            checkExpressionHasType(statement.rhs, annotatedType)
        }
    }

    private fun Type.isAssignableTo(destination: Type): Boolean {
        return ctx.analyzer.isTypeAssignableTo(source = this, destination = destination)
    }

    private fun checkExpression(expression: Expression) {
        exhaustive(when (expression) {
            is Expression.Error -> unit
            is Expression.Var -> checkVarExpression(expression)
            is Expression.Call -> checkCallExpression(expression)
            is Expression.Property -> checkPropertyExpression(expression)
            is Expression.ByteString -> Unit
            is Expression.BoolLiteral -> Unit
            is Expression.NullPtr -> checkNullPtrExpression(expression)
            is Expression.IntLiteral -> Unit
            is Expression.Not -> TODO()
            is Expression.BinaryOperation -> checkBinaryOperation(expression)
            is Expression.PipelineOperator -> checkPipelineOperator(expression)
            is Expression.SizeOf -> checkSizeOf(expression)
            is Expression.AddressOf -> checkAddressOf(expression)
            is Expression.AddressOfMut -> checkAddressOfMut(expression)
            is Expression.Deref -> checkDeref(expression)
            is Expression.PointerCast -> checkPointerCast(expression)
            is Expression.If -> checkIfExpression(expression)
            is Expression.TypeApplication -> checkTypeApplicationExpression(expression)
            is Expression.New -> TODO()
            is Expression.This -> checkThisExpression(expression)
            is Expression.Closure -> TODO()
            is Expression.UnsafeCast -> checkUnsafeCast(expression)
            is Expression.When -> checkWhenExpression(expression)
        })
    }

    private fun checkThisExpression(expression: Expression.This) {

        val extension = ctx.resolver.getEnclosingExtensionDef(expression)

        if (extension == null) {
            error(expression, Diagnostic.Kind.UnboundThis)
        }
    }

    private fun checkWhenExpression(expression: Expression.When) {
        checkExpression(expression.value)
        expression.arms.forEach {
            checkExpression(it.value)
        }

        val discriminants = ctx.analyzer.getDiscriminants(expression.value.type)

    }

    private fun checkTypeApplicationExpression(expression: Expression.TypeApplication) {
        expression.args.forEach { checkTypeAnnotation(it) }
        val traitRef = ctx.analyzer.resolveTraitRef(expression.lhs)
        if (traitRef != null) {
            if (traitRef.params.size > expression.args.size) {
                error(expression.lhs, Diagnostic.Kind.TooFewTypeArgs)
            } else if (traitRef.params.size < expression.args.size) {
                error(expression.lhs, Diagnostic.Kind.TooManyTypeArgs)
            }
            return
        }

        checkExpression(expression.lhs)
        val lhsType = expression.lhs.type
        if (lhsType !is Type.TypeFunction) {
            error(expression.lhs, Diagnostic.Kind.InvalidTypeApplication)
            return
        }
        if (expression.args.size != lhsType.params.size) {
            error(expression.lhs, Diagnostic.Kind.InvalidTypeApplication)
        }
    }

    private fun checkPointerCast(expression: Expression.PointerCast) {
        checkExpression(expression.arg)
        checkTypeAnnotation(expression.toType)
        if (expression.arg.type !is Type.Ptr) {
            error(expression.arg, Diagnostic.Kind.NotAPointerType(expression.arg.type))
        }
    }

    private fun checkUnsafeCast(expression: Expression.UnsafeCast) {
        checkExpression(expression.value)
        checkTypeAnnotation(expression.toType)
    }

    private fun checkDeref(expression: Expression.Deref) {
        checkExpression(expression.expression)
        if (expression.expression.type !is Type.Ptr) {
            error(expression.expression, Diagnostic.Kind.NotAPointerType(expression.expression.type))
        }
    }

    private fun checkSizeOf(expression: Expression.SizeOf) {
        checkTypeAnnotation(expression.type)
    }

    private fun checkAddressOfMut(expression: Expression.AddressOfMut) {
        checkExpression(expression.expression)
        checkValueIsAddressable(expression.expression)
        if (expression.expression is Expression.Var) {
            val binding = ctx.resolver.resolve(expression.expression.name)
            if (binding is Binding.ValBinding) {
                if (!binding.statement.isMutable) {
                    error(expression.expression, Diagnostic.Kind.ValNotMutable)
                }
            }
        }
    }

    private fun checkAddressOf(expression: Expression.AddressOf) {
        checkExpression(expression.expression)
        checkValueIsAddressable(expression.expression)
    }

    private fun checkValueIsAddressable(expression: Expression) {
        if (expression !is Expression.Var) {
            error(expression, Diagnostic.Kind.NotAnAddressableValue)
            return
        }
        val binding = ctx.resolver.resolve(expression.name)
        if (binding !is Binding.ValBinding) {
            error(expression, Diagnostic.Kind.NotAnAddressableValue)
            return
        }
    }

    private fun checkPipelineOperator(expression: Expression.PipelineOperator) {
        checkCallLikeExpression(
            callExpression= expression,
            typeArgAnnotations = null,
            callee = expression.rhs,
            args = listOf(expression.lhs)
        )
    }

    private fun checkIfExpression(expression: Expression.If) {
        checkExpressionHasType(expression.condition, Type.Bool)
        checkExpression(expression.trueBranch)
        checkExpression(expression.falseBranch)

        checkExpressionHasType(expression.falseBranch, expression.trueBranch.type)

    }

    private val Expression.type get() = ctx.analyzer.typeOfExpression(this)

    private fun checkNullPtrExpression(expression: Expression.NullPtr) {

    }

    private fun checkBinaryOperation(expression: Expression.BinaryOperation) {

    }

    private fun checkPropertyExpression(expression: Expression.Property) {
        val moduleProperty = ctx.resolver.resolveModuleProperty(expression)
        if (moduleProperty != null) {
            return
        }

        checkExpression(expression.lhs)
        if (!ctx.analyzer.isValidPropertyAccess(expression)) {
            val lhsType = ctx.analyzer.typeOfExpression(expression.lhs)
            if (lhsType is Type.Error) return
            error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
        }
    }

    private fun checkVarExpression(expression: Expression.Var) {
        val resolved = ctx.resolver.resolve(expression.name)
        if (resolved != null) return

        val traitRef = ctx.resolver.resolveTraitDef(expression.name)
        if (traitRef != null) return
        error(expression.name, Diagnostic.Kind.UnboundVariable(expression.name.name))
    }

    private fun checkCallLikeExpression(
        callExpression: Expression,
        callee: Expression,
        args: List<Expression>,
        typeArgAnnotations: List<TypeAnnotation>?
    ) {

        if (callee is Expression.Property) {
            val propertyBinding = ctx.analyzer.resolvePropertyBinding(callee)
            if (propertyBinding is PropertyBinding.TraitFunctionRef) {
                val requirement = TraitRequirement(
                    propertyBinding.traitName,
                    propertyBinding.args
                )
                val traitDef = ctx.resolver.resolveDeclaration(requirement.traitRef)
                require(traitDef is Declaration.TraitDef)

                if (traitDef.params.size == propertyBinding.args.size &&
                    !ctx.analyzer.isTraitRequirementSatisfied(callExpression, requirement)) {
                    error(callee.lhs, Diagnostic.Kind.TraitRequirementNotSatisfied(requirement))
                }
            }
        }
        checkExpression(callee)
        args.forEach { checkExpression(it) }
        typeArgAnnotations?.forEach { checkTypeAnnotation(it) }

        val calleeType = ctx.analyzer.getCalleeType(callExpression)
        val fnTypeComponents = ctx.analyzer.getFunctionTypeComponents(calleeType)
        if (fnTypeComponents == null) {
            if (calleeType != Type.Error) {
                error(callee, Diagnostic.Kind.TypeNotCallable(calleeType))
            }
            return
        }

        val receiver = ctx.analyzer.getCallReceiver(callExpression)
        val expectedArgSize = if (receiver == null) fnTypeComponents.from.size else fnTypeComponents.from.size - 1
        if (args.size > expectedArgSize) {
            error(callee, Diagnostic.Kind.TooManyArgs(expectedArgSize))
        } else if (args.size < expectedArgSize) {
            error(callee, Diagnostic.Kind.MissingArgs(expectedArgSize))
        }

        if (typeArgAnnotations != null) {
            val expectedTypeArgs = fnTypeComponents.typeParams?.size ?: 0
            if (expectedTypeArgs < typeArgAnnotations.size) {
                error(callee, Diagnostic.Kind.TooManyTypeArgs)
            }
        }
        val typeArgs = ctx.analyzer.getTypeArgs(callExpression)
        if (fnTypeComponents.traitRequirements != null) {
            val substitution = (fnTypeComponents.typeParams?: emptyList()).zip(typeArgs ?: emptyList())
                .map { (p, arg) -> p.binder.location to arg }
                .toMap()

            checkTraitInstances(
                callExpression,
                fnTypeComponents.traitRequirements.map { it.applySubstitution(substitution) }
            )
        }

        val fromTypes =
            if (receiver != null) {
                fnTypeComponents.from.drop(1)
            } else {
                fnTypeComponents.from
            }
        val substitution = fnTypeComponents.typeParams?.zip(typeArgs ?: emptyList())
            ?.map { (param, type) -> param.binder.location to type }
            ?.toMap()
            ?: emptyMap()
        fromTypes.zip(args).forEach { (expectedType, arg) ->
            checkExpressionHasType(arg, expectedType.applySubstitution(substitution))
        }
    }

    private fun checkCallExpression(expression: Expression.Call) {
        return checkCallLikeExpression(
            callee = expression.callee,
            callExpression = expression,
            args = expression.args.map { it.expression },
            typeArgAnnotations = expression.typeArgs
        )
    }


    private fun checkTraitInstances(callNode: Expression, requiredInstances: List<TraitRequirement>) {
        for (requirement in requiredInstances) {
            if (!ctx.analyzer.isTraitRequirementSatisfied(callNode, requirement)) {
                error(callNode, Diagnostic.Kind.TraitRequirementNotSatisfied(requirement))
            }
        }
    }

    private fun checkFunctionSignature(signature: FunctionSignature, skipThisParamCheck: Boolean = false) {
        if (ctx.resolver.getEnclosingExtensionDef(signature) != null) {
            checkTopLevelExpressionBinding(signature.name)

            if (signature.thisParamFlags == null) {
                error(signature.name, Diagnostic.Kind.MissingThisParam)
            }
        } else {
            if (!skipThisParamCheck && signature.thisParamFlags != null) {
                error(signature, Diagnostic.Kind.UnboundThis)
            }
        }
        checkFunctionParams(signature.params)
        signature.typeParams?.let {
            checkTypeParams(it)
        }
        checkTypeAnnotation(signature.returnType)
    }

    private fun checkFunctionParams(params: List<Param>) {
        val binders = mutableMapOf<Name, Binder>()
        for (param in params) {
            if (param.annotation == null) {
                error(param, Diagnostic.Kind.MissingTypeAnnotation)
            } else {
                checkTypeAnnotation(param.annotation)
            }

            val existing = binders[param.binder.name]
            if (existing != null) {
                error(param.binder, Diagnostic.Kind.DuplicateValueBinding(existing))
            } else{
                binders[param.binder.name] = param.binder
            }
        }
    }

    private fun checkImportAsDeclaration(declaration: Declaration.ImportAs) {
        val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
        if (sourceFile == null) {
            error(declaration.modulePath, Diagnostic.Kind.NoSuchModule)
            return
        }
    }

    private fun checkImportMembersDeclaration(declaration: Declaration.ImportMembers) {
        val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
        if (sourceFile == null) {
            error(declaration.modulePath, Diagnostic.Kind.NoSuchModule)
            return
        }
        declaration.names.forEach { name ->
            val declaredExpression = ctx.resolver.findInSourceFile(name.name, sourceFile)
            val declaredType = ctx.resolver.findTypeInSourceFile(name.identifier, sourceFile)

            if (declaredExpression == null && declaredType == null) {
                error(name, Diagnostic.Kind.NoSuchMember)
            }
            if (declaredExpression != null) {
                checkTopLevelExpressionBinding(name)
            }
            if (declaredType != null) {
                checkTopLevelTypeBinding(name)
            }
        }
    }

    private val topLevelExpressionBindingsByFile = mutableMapOf<SourcePath, MutableMap<Name, Binder>>()
    private fun checkTopLevelExpressionBinding(binder: Binder) {
        val topLevelExpressionBindings = topLevelExpressionBindingsByFile.getOrPut(binder.location.file) { mutableMapOf() }
        if (binder.name in topLevelExpressionBindings) {
            error(binder, Diagnostic.Kind.DuplicateValueBinding(binder))
        } else {
            topLevelExpressionBindings[binder.name] = binder
        }
    }

    private val topLevelTypeBindingsBySourcePath = mutableMapOf<SourcePath, MutableMap<Name, Binder>>()
    private fun checkTopLevelTypeBinding(binder: Binder) {
        val topLevelTypeBindings = topLevelTypeBindingsBySourcePath.getOrPut(binder.location.file) { mutableMapOf() }
        binder.location.file
        if (binder.name in topLevelTypeBindings) {
            error(binder, Diagnostic.Kind.DuplicateValueBinding(binder))
        } else {
            topLevelTypeBindings[binder.name] = binder
        }
    }

    private fun error(node: HasLocation, diagnostic: Diagnostic.Kind) {
        ctx.diagnosticReporter.report(node.location, diagnostic)
    }
}