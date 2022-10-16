package hadesc.frontend

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourcePath
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.types.emptySubstitution
import hadesc.types.toSubstitution
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
        is Declaration.Enum -> checkEnumDef(declaration)
        is Declaration.ExternConst -> checkExternConstDef(declaration)
    }

    private fun checkEnumDef(declaration: Declaration.Enum) {
        declaration.typeParams?.let { checkTypeParams(it) }
        checkTopLevelTypeBinding(declaration.name)
        val caseNames = mutableSetOf<Name>()
        declaration.cases.forEach { case ->
            case.params?.forEach {
                checkTypeAnnotation(it.annotation)
            }
            if (case.name.name in caseNames) {
                error(case.name, Diagnostic.Kind.DuplicateVariantName)
            } else {
                caseNames.add(case.name.name)
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
        val expectedAssociatedTypes = if (traitDef is Declaration.TraitDef) {
            traitDef.expectedAssociatedTypes()
        } else {
            emptyMap()
        }
        val foundMethods = mutableSetOf<Name>()
        val foundAssociatedTypes = mutableSetOf<Name>()
        val associatedTypeSubstitution =
            implDef.body.filterIsInstance<Declaration.TypeAlias>().associate {
                requireNotNull(expectedAssociatedTypes[it.name.name]).location to ctx.analyzer.annotationToType(it.rhs)
            }.toSubstitution()

        for (declaration in implDef.body) {
            if (declaration is Declaration.FunctionDef) {
                checkFunctionDef(declaration, skipDuplicateDeclarationCheck = false)
                foundMethods.add(declaration.name.identifier.name)
                val typeOfMethod = ctx.analyzer.typeOfBinder(declaration.name)
                require(typeOfMethod is Type.Ptr)
                val actualType = typeOfMethod.to
                val expectedType = expectedMethods[declaration.name.identifier.name]?.applySubstitution(associatedTypeSubstitution)
                if (expectedType != null && !actualType.isAssignableTo(declaration, expectedType)) {
                    error(declaration.name, Diagnostic.Kind.TraitMethodTypeMismatch(
                        expected = expectedType, found = actualType))
                }
            } else if (declaration is Declaration.TypeAlias) {
                checkTypeAlias(declaration, skipDuplicateDeclarationCheck = true)
                foundAssociatedTypes.add(declaration.name.name)
                checkTypeAnnotation(declaration.rhs)
                require(declaration.typeParams == null)
            } else {
                error(declaration.startLoc, Diagnostic.Kind.OnlyFunctionDefsAndTypeAliasesAllowedInImplementationDefs)
            }
        }

        for (name in expectedMethods.keys) {
            if (name !in foundMethods) {
                error(implDef.traitRef, Diagnostic.Kind.MissingImplMethod(name))
            }
        }

        for (name in expectedAssociatedTypes.keys) {
            if (name !in foundAssociatedTypes) {
                error(implDef.traitRef, Diagnostic.Kind.MissingAssociatedType(name))
            }
        }
    }

    private fun Declaration.TraitDef.expectedAssociatedTypes(): Map<Name, Binder> =
        members.filterIsInstance<Declaration.TraitMember.AssociatedType>()
            .associate { it.binder.identifier.name to it.binder }

    private fun Declaration.TraitDef.expectedMethods(typeArguments: List<Type>): Map<Name, Type> {
        val map = mutableMapOf<Name, Type>()
        val substitution = params.zip(typeArguments).toSubstitution()
        for (method in signatures) {
            val paramTypes = method.params
                .map { it.annotation to it.location }
                .map { (it, location) -> it?.type ?: Type.Error(location) }
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

    private fun checkExternConstDef(declaration: Declaration.ExternConst) {
        checkTypeAnnotation(declaration.type)

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

    private fun checkTypeAlias(declaration: Declaration.TypeAlias, skipDuplicateDeclarationCheck: Boolean = false) {
        if (!skipDuplicateDeclarationCheck) {
            // TODO: Check for duplicate declarations in non top level scopes
            //       Right now, this would fail at codegen time.
            //       Also need to do this for checkFunctionDef
            checkTopLevelTypeBinding(declaration.name)
        }
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
            checkReturnType(annotation.to, annotation.to.type)
            checkTypeAnnotation(annotation.to)

            unit
        }
        is TypeAnnotation.Union -> {
            annotation.args.forEach {
                checkTypeAnnotation(it)
            }
        }
        is TypeAnnotation.Select -> {
            checkSelectTypeAnnotation(annotation)
        }
    }

    private fun checkSelectTypeAnnotation(annotation: TypeAnnotation.Select) {
        val traitRequirement = ctx.analyzer.asTraitRequirement(annotation.lhs)
        if (traitRequirement == null) {
            error(annotation.lhs, Diagnostic.Kind.NotATrait)
            return
        }
        val traitDecl = ctx.analyzer.getTraitDef(traitRequirement)
        require(traitDecl is Declaration.TraitDef) // otherwise asTraitRequirement would be null
        if (!traitDecl.hasAssociatedType(annotation.rhs)) {
            error(annotation.rhs, Diagnostic.Kind.NoSuchAssociatedType(annotation.rhs.name))
        }
    }

    private fun checkReturnType(node: HasLocation, type:Type) {
        checkReturnTypeWorker(node, ctx.analyzer.reduceGenericInstances(type))
    }
    private fun checkReturnTypeWorker(node: HasLocation, unreducedType: Type, typeArguments: List<Type>? = null): Unit = when (val type = ctx.analyzer.reduceGenericInstances(unreducedType)) {
        is Type.Application -> {
            checkReturnTypeWorker(node, type.callee, typeArguments = type.args)
        }
        is Type.Constructor -> {

            when (val declaration = ctx.resolver.resolveDeclaration(type.name)) {
                is Declaration.Struct -> {

                    val substitution =
                        if (typeArguments != null && declaration.typeParams != null)
                            declaration.typeParams.zip(typeArguments).toSubstitution()
                        else
                            emptySubstitution()
                    val fieldTypes = declaration.members.filterIsInstance<Declaration.Struct.Member.Field>()
                        .map { it.typeAnnotation.type }
                        .map { it.applySubstitution(substitution) }
                    fieldTypes.forEach { checkReturnTypeWorker(node, it) }
                }
                is Declaration.Enum -> {

                    val substitution =
                        if (typeArguments != null && declaration.typeParams != null)
                            declaration.typeParams.zip(typeArguments).toSubstitution()
                        else
                            emptySubstitution()
                    val memberTypes = declaration.cases
                        .asSequence()
                        .mapNotNull { it.params }
                        .flatten()
                        .map { it.annotation }
                        .map { it.type }
                        .map { it.applySubstitution(substitution) }
                    memberTypes.forEach { checkReturnTypeWorker(node, it) }
                }
                is Declaration.TypeAlias -> requireUnreachable()
                else -> unit
            }
        }
        is Type.TypeFunction -> TODO()
        is Type.Function -> {
            error(node, Diagnostic.Kind.ReturnTypeMustNotContainClosuresOrRefs)
        }
        is Type.GenericInstance -> requireUnreachable()
        is Type.UntaggedUnion -> {
            type.members.forEach {
                checkReturnTypeWorker(node, it)
            }
        }
        else -> unit
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

    private fun checkFunctionDef(declaration: Declaration.FunctionDef, skipDuplicateDeclarationCheck: Boolean = false) {
        if (!skipDuplicateDeclarationCheck) {
            checkFunctionSignature(declaration.signature)
        }
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
        checkNoCopy(statement.value)

        checkExpressionHasType(statement.value, statement.lhs.type)

        val field = ctx.analyzer.resolvePropertyBinding(statement.lhs)
        if (field !is PropertyBinding.StructField && field !is PropertyBinding.StructPointerFieldLoad) {
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
        checkNoCopy(statement.value)

        when (val lhsType = statement.lhs.expression.type) {
            is Type.Ptr -> {
                if (!lhsType.isMutable) {
                    error(statement.lhs, Diagnostic.Kind.ValNotMutable)
                }
                checkExpressionHasType(statement.value, lhsType.to)
            }
            else -> {
                error(statement.lhs, Diagnostic.Kind.NotAPointerType(lhsType))
            }
        }

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
        checkNoCopy(statement.value)
    }

    private fun checkWhileStatement(statement: Statement.While) {
        checkExpressionHasType(statement.condition, Type.Bool)
        checkBlock(statement.body)
    }

    private fun checkIfStatement(statement: Statement.If) {
        checkExpression(statement.condition)
        checkExpressionHasType(statement.condition, Type.Bool)
        checkBlock(statement.ifTrue)
        statement.ifFalse?.let { checkBlock(it) }
    }

    private fun checkExpressionHasType(expression: Expression, type: Type) {
        checkExpression(expression)
        val exprType = ctx.analyzer.typeOfExpression(expression)
        if (!exprType.isAssignableTo(expression, type)) {
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

        if (expectedReturnType.isAssignableTo(statement, Type.Void)
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
        checkNoCopy(statement.rhs)

        if (statement.typeAnnotation != null) {
            val annotatedType = ctx.analyzer.annotationToType(statement.typeAnnotation)
            checkExpressionHasType(statement.rhs, annotatedType)
        }
    }

    private fun checkNoCopy(expr: Expression) {
        if (!ctx.analyzer.isCopyAllowed(expr, expr.type) && !ctx.analyzer.isRValue(expr)) {
            error(expr, Diagnostic.Kind.CopyOfNoCopyTypeNotAllowed)
        }
    }

    private fun Type.isAssignableTo(at: HasLocation, destination: Type): Boolean {
        return ctx.analyzer.isTypeAssignableTo(at, source = this, destination = destination)
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
            is Expression.FloatLiteral -> unit
            is Expression.Not -> checkNotExpression(expression)
            is Expression.BinaryOperation -> checkBinaryOperation(expression)
            is Expression.SizeOf -> checkSizeOf(expression)
            is Expression.AlignOf -> checkAlignOf(expression)
            is Expression.AddressOf -> checkAddressOf(expression)
            is Expression.AddressOfMut -> checkAddressOfMut(expression)
            is Expression.Deref -> checkDeref(expression)
            is Expression.PointerCast -> checkPointerCast(expression)
            is Expression.If -> checkIfExpression(expression)
            is Expression.TypeApplication -> checkTypeApplicationExpression(expression)
            is Expression.Closure -> checkClosureExpression(expression)
            is Expression.This -> checkThisExpression(expression)
            is Expression.As -> checkAsExpression(expression)
            is Expression.BlockExpression -> checkBlockExpression(expression)
            is Expression.Intrinsic -> checkIntrinsicExpression(expression)
            is Expression.UnaryMinus -> checkUnaryMinusExpression(expression)
            is Expression.ByteCharLiteral -> unit
            is Expression.Match -> checkMatchExpression(expression)
            is Expression.Uninitialized -> unit
            is Expression.Move -> checkMoveExpression(expression)
        })
    }

    private fun checkMoveExpression(expression: Expression.Move) {

        when (val binding = ctx.resolver.resolve(expression.name)) {
            is Binding.Local -> {
                val enclosingClosure = ctx.resolver.getEnclosingClosure(expression)
                if (enclosingClosure != null && !binding.isLocalTo(enclosingClosure)) {
                    error(
                        expression.name,
                        Diagnostic.Kind.UseAfterMove(
                            expression.name.location,
                            hint="Since closures might be called multiple times, we have to " +
                                    "assume that it might already be moved on subsequent invocations."
                        )
                    )
                }

                val enclosingWhile = ctx.resolver.getEnclosingWhileLoop(expression)
                if (enclosingWhile != null && !binding.isLocalTo(enclosingWhile)) {
                    error(
                        expression.name,
                        Diagnostic.Kind.UseAfterMove(
                            expression.name.location,
                            hint="Moved in the previous iteration of the loop."
                        )
                    )
                }

            }
            null -> {
                error(expression, Diagnostic.Kind.UnboundVariable(expression.name.name))
            }
            else -> {
                error(expression.name, Diagnostic.Kind.CantMoveNonLocal)
            }
        }
    }

    private fun checkMatchExpression(expression: Expression.Match) {
        checkExpression(expression.value)
        val expectedType = expression.arms.firstOrNull()?.value?.type
        if (expectedType != null) {
            expression.arms.forEach {
                checkExpressionHasType(it.value, expectedType)
            }
        }

        val discriminantType = expression.value.type
        if (!discriminantType.isIntegral() && !ctx.analyzer.isEnumType(discriminantType)) {
            error(expression.value, Diagnostic.Kind.NotAMatchableType(discriminantType))
        }
        expression.arms.forEach {
            checkPatternHasType(it.pattern, expression.value.type)
        }

        checkPatternExhaustivity(expression.value, expression.value.type, expression.arms)

    }

    private fun checkPatternHasType(pattern: Pattern, type: Type) {
        when (pattern) {
            is Pattern.IntLiteral -> {
                if (type !is Type.Integral) {
                    error(pattern, Diagnostic.Kind.NotAnIntegralValue)
                }
            }
            is Pattern.EnumCase -> {
                val enumDeclaration = ctx.analyzer.getEnumTypeDeclaration(type)
                if (enumDeclaration == null) {
                    error(pattern, Diagnostic.Kind.NotAnEnumType(type))
                    return
                }
                val variants = ctx.analyzer.getEnumDiscriminants(enumDeclaration, type.typeArgs())
                if (variants.none { it.name == pattern.identifier.name }) {
                    error(pattern, Diagnostic.Kind.NoSuchCase(enumDeclaration, pattern.identifier.name))
                }
            }
            is Pattern.Wildcard -> {}
            is Pattern.Val -> {}
        }
    }

    private fun checkPatternExhaustivity(location: HasLocation, valueType: Type, arms: List<Expression.Match.Arm>) {
        if (valueType.isIntegral()) {
            if (arms.none { it.pattern is Pattern.Wildcard }) {
                error(location, Diagnostic.Kind.NonExhaustivePrimitivePatterns)
            }
            return
        }

        val enumDecl = ctx.analyzer.getEnumTypeDeclaration(valueType)
        if (enumDecl != null) {
            if (arms.any { it.pattern is Pattern.Wildcard }) {
                return
            }

            val handledCases = mutableSetOf<Name>()
            for (arm in arms) {
                if (arm.pattern is Pattern.EnumCase) {
                    handledCases.add(arm.pattern.identifier.name)
                }
            }

            for (case in enumDecl.cases) {
                if (case.name.name !in handledCases) {
                    error(location, Diagnostic.Kind.NonExhaustivePatterns(case.name.name))
                }
            }
        }
    }

    private fun checkUnaryMinusExpression(expression: Expression.UnaryMinus) {
        val type = expression.expression.type
        if (type is Type.Integral && type.isSigned) {
            return
        }
        if (type is Type.Size && type.isSigned) {
            return
        }
        if (type is Type.FloatingPoint) {
            return
        }

        error(expression, Diagnostic.Kind.TypeDoesNotSupportArithmetic(type))
    }

    private fun checkIntrinsicExpression(expression: Expression.Intrinsic) {
        return when (expression.intrinsicType) {
            IntrinsicType.ADD, IntrinsicType.SUB, IntrinsicType.MUL -> {
                val typeArgs = ctx.analyzer.getTypeArgs(expression) ?: return

                if (typeArgs.size != 1) {
                    return
                }

                when (val typeArg = typeArgs.first()) {
                    is Type.Integral,
                    is Type.Size -> unit
                    else -> error(expression, Diagnostic.Kind.TypeDoesNotSupportArithmetic(typeArg))
                }


                unit
            }
            IntrinsicType.PTR_TO_INT -> unit
            IntrinsicType.INT_TO_PTR -> {
                val typeArgs = ctx.analyzer.getTypeArgs(expression) ?: return

                if (typeArgs.size != 1) {
                    return
                }

                when (val typeArg = typeArgs.first()) {
                    is Type.Ptr, is Type.Error -> unit
                    else -> error(expression, Diagnostic.Kind.NotAPointerType(typeArg))
                }
                unit
            }
            IntrinsicType.MEMCPY -> unit
            IntrinsicType.ERROR -> unit
        }
    }

    private fun checkBlockExpression(expression: Expression.BlockExpression) {
        checkBlock(expression.block)
    }

    private fun checkAsExpression(expression: Expression.As) {
        checkExpression(expression.lhs)
        checkTypeAnnotation(expression.rhs)

        val lhsType = expression.lhs.type


        if (!lhsType.isIntegral()) {
            error(expression.lhs, Diagnostic.Kind.NotAnIntegralValue)
        }

        if (!expression.rhs.type.isIntegral()) {
            error(expression.rhs, Diagnostic.Kind.NotAnIntegralValue)
        }
    }

    private fun checkNotExpression(expression: Expression.Not) {
        checkExpressionHasType(expression.expression, Type.Bool)
    }

    private fun checkClosureExpression(expression: Expression.Closure) {
        val expressionType = expression.type
        require(expressionType is Type.Function)
        val returnType = expressionType.to
        checkReturnType(expression.returnType ?: expression.body, returnType)
        returnTypeStack.push(returnType)
        checkFunctionParams(expression.params)
        expression.returnType?.let { checkTypeAnnotation(it) }

        when (expression.body) {
            is ClosureBody.Block -> checkBlock(expression.body.block)
            is ClosureBody.Expression -> checkExpressionHasType(expression.body.expression, returnType)
        }
        returnTypeStack.pop()
    }

    private fun checkThisExpression(expression: Expression.This) {

        val extension = ctx.resolver.getEnclosingExtensionDef(expression)

        if (extension == null) {
            error(expression, Diagnostic.Kind.UnboundThis)
        }
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

    private fun checkDeref(expression: Expression.Deref) {
        checkExpression(expression.expression)
        if (expression.expression.type !is Type.Ptr) {
            error(expression.expression, Diagnostic.Kind.NotAPointerType(expression.expression.type))
        }
    }

    private fun checkSizeOf(expression: Expression.SizeOf) {
        checkTypeAnnotation(expression.type)
    }
    private fun checkAlignOf(expression: Expression.AlignOf) {
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

        when (expression) {
            is Expression.Property -> {
                when (ctx.analyzer.resolvePropertyBinding(expression)) {
                    is PropertyBinding.StructField -> {
                        checkValueIsAddressable(expression.lhs)
                    }
                    is PropertyBinding.StructPointerFieldLoad -> unit
                    else -> {
                        error(expression, Diagnostic.Kind.NotAnAddressableValue)
                    }
                }
            }
            is Expression.Var -> {
                if (ctx.resolver.resolve(expression.name) !is Binding.ValBinding) {
                    error(expression, Diagnostic.Kind.NotAnAddressableValue)
                }
            }
            else -> {
                error(expression, Diagnostic.Kind.NotAnAddressableValue)
            }
        }
        if (expression.type is Type.Function) {
            error(expression, Diagnostic.Kind.TakingAddressOfClosureDisallowed)
        }
    }

    private fun checkIfExpression(expression: Expression.If) {
        checkExpressionHasType(expression.condition, Type.Bool)
        checkExpression(expression.trueBranch)
        checkExpression(expression.falseBranch)

        checkExpressionHasType(expression.falseBranch, expression.trueBranch.type)

    }

    private val Expression.type get() = ctx.analyzer.typeOfExpression(this)

    private fun checkNullPtrExpression(
        @Suppress("UNUSED_PARAMETER")
        expression: Expression.NullPtr) = unit

    private fun checkBinaryOperation(expression: Expression.BinaryOperation) {
        checkExpression(expression.lhs)
        checkExpression(expression.rhs)
        if (expression.type is Type.Error) {
            error(expression, Diagnostic.Kind.OperatorNotApplicable(expression.operator, expression.lhs.type, expression.rhs.type))
        }
    }

    private fun checkPropertyExpression(expression: Expression.Property) {
        val moduleProperty = ctx.resolver.resolveModuleProperty(expression)
        if (moduleProperty == null) {
            checkExpression(expression.lhs)
            if (!ctx.analyzer.isValidPropertyAccess(expression)) {
                val lhsType = ctx.analyzer.typeOfExpression(expression.lhs)
                if (lhsType !is Type.Error) {
                    error(expression.property, Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
                }
            }
        }

        val lhsBinding = when (val lhs = expression.lhs) {
            is Expression.Property -> {
                ctx.resolver.resolveModuleProperty(lhs)
            }
            is Expression.Var -> {
                ctx.resolver.resolve(lhs.name)
            }
            else -> null
        }
        if (lhsBinding != null && lhsBinding is Binding.Enum) {
            if (lhsBinding.declaration.cases.none { it.name.name == expression.property.name }) {
                error(expression.property, Diagnostic.Kind.NoSuchMember)
            }
        }
    }

    private fun checkVarExpression(expression: Expression.Var) {
        val resolved = ctx.resolver.resolve(expression.name)
        if (resolved != null) {
            return
        }

        val traitRef = ctx.resolver.resolveTraitDef(expression.name)
        if (traitRef != null) return
        error(expression.name, Diagnostic.Kind.UnboundVariable(expression.name.name))
    }

    private fun checkCallLikeExpression(
        callExpression: Expression.Call,
        callee: Expression,
        args: List<Expression>,
    ) {

        ctx.analyzer.typeOfExpression(callExpression)
        if (callee is Expression.Property) {
            val propertyBinding = ctx.analyzer.resolvePropertyBinding(callee)
            if (propertyBinding is PropertyBinding.TraitFunctionRef) {
                val requirement = TraitRequirement(
                    propertyBinding.traitName,
                    propertyBinding.args,
                    negated = false,
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
        args.forEach {
            checkExpression(it)
            checkNoCopy(it)
        }

        val calleeType = ctx.analyzer.getCalleeType(callExpression)
        val fnTypeComponents = ctx.analyzer.getFunctionTypeComponents(calleeType)
        if (fnTypeComponents == null) {
            if (calleeType !is Type.Error) {
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

        val genericCallee = when (callee) {
            is Expression.TypeApplication -> callee.lhs
            else -> callee
        }
        val typeArgs = ctx.analyzer.getTypeArgs(genericCallee)
        if (fnTypeComponents.traitRequirements != null) {
            val substitution = (fnTypeComponents.typeParams?: emptyList()).zip(typeArgs ?: emptyList()).toSubstitution()

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
            ?.toSubstitution()
            ?: emptySubstitution()
        fromTypes.zip(args).forEach { (expectedType, arg) ->
            checkExpressionHasType(arg, expectedType.applySubstitution(substitution))
        }
    }

    private fun checkCallExpression(expression: Expression.Call) {
        return checkCallLikeExpression(
            callee = expression.callee,
            callExpression = expression,
            args = expression.args.map { it.expression },
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
        checkReturnType(signature.returnType, signature.returnType.type)
        checkTypeAnnotation(signature.returnType)
    }

    private fun checkFunctionParams(params: List<Param>) {
        val binders = mutableMapOf<Name, Binder>()
        for (param in params) {
            if (param.annotation == null) {
                if (ctx.analyzer.getInferredParamType(param) == null) {
                    error(param, Diagnostic.Kind.MissingTypeAnnotation)
                }
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