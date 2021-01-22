package hadesc.frontend

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TypeAnalyzer
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.location.HasLocation
import hadesc.resolver.Binding
import hadesc.types.Type
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
        is Declaration.ConstDefinition -> TODO()
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.Struct -> checkStructDeclaration(declaration)
        is Declaration.TypeAlias -> checkTypeAlias(declaration)
        is Declaration.ExtensionDef -> checkExtensionDef(declaration)
        is Declaration.TraitDef -> TODO()
        is Declaration.ImplementationDef -> TODO()
        is Declaration.SealedType -> TODO()
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
        is TypeAnnotation.Error -> {}
        is TypeAnnotation.Var -> {
            val resolved = ctx.resolver.resolveTypeVariable(annotation.name)
            if (resolved == null) {
                error(annotation, Diagnostic.Kind.UnboundType(annotation.name.name))
            }
            Unit
        }
        is TypeAnnotation.Ptr -> {
            checkTypeAnnotation(annotation.to)
        }
        is TypeAnnotation.MutPtr -> {
            checkTypeAnnotation(annotation.to)
        }
        is TypeAnnotation.Application -> {
            TODO()
        }
        is TypeAnnotation.Qualified -> {
            val binding = ctx.resolver.resolveQualifiedType(annotation.qualifiedPath)
            if (binding == null) {
                error(annotation, Diagnostic.Kind.UnboundTypePath(annotation.qualifiedPath))
            }
            Unit
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
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Defer -> checkDeferStatement(statement)
        is Statement.Error -> Unit
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
            val actualType = ctx.analyzer.typeOfExpression(statement.value)
            if (!actualType.isAssignableTo(expectedReturnType)) {
                error(statement, Diagnostic.Kind.TypeNotAssignable(source = actualType, destination = expectedReturnType))
            }
        }
    }

    private fun checkValStatement(statement: Statement.Val) {
        statement.typeAnnotation?.let {
            checkTypeAnnotation(it)
        }
        checkExpression(statement.rhs)

        if (statement.typeAnnotation != null) {
            val typeOfRhs = ctx.analyzer.typeOfExpression(statement.rhs)
            val annotatedType = ctx.analyzer.annotationToType(statement.typeAnnotation)
            if (!typeOfRhs.isAssignableTo(annotatedType)) {
                error(statement.rhs, Diagnostic.Kind.TypeNotAssignable(source = typeOfRhs, destination = annotatedType))
            }
        }
    }

    private fun Type.isAssignableTo(destination: Type): Boolean {
        return TypeAnalyzer().isTypeAssignableTo(source = this, destination = destination)
    }

    private fun checkExpression(expression: Expression): Unit = when(expression) {
        is Expression.Error -> TODO()
        is Expression.Var -> checkVarExpression(expression)
        is Expression.Call -> checkCallExpression(expression)
        is Expression.Property -> checkPropertyExpression(expression)
        is Expression.ByteString -> Unit
        is Expression.BoolLiteral -> Unit
        is Expression.NullPtr -> checkNullPtrExpression(expression)
        is Expression.IntLiteral -> Unit
        is Expression.Not -> TODO()
        is Expression.BinaryOperation -> checkBinaryOperation(expression)
        is Expression.PipelineOperator -> TODO()
        is Expression.SizeOf -> TODO()
        is Expression.AddressOf -> TODO()
        is Expression.AddressOfMut -> TODO()
        is Expression.Deref -> TODO()
        is Expression.PointerCast -> TODO()
        is Expression.If -> TODO()
        is Expression.TypeApplication -> TODO()
        is Expression.New -> TODO()
        is Expression.This -> TODO()
        is Expression.Closure -> TODO()
        is Expression.TraitMethodCall -> TODO()
        is Expression.UnsafeCast -> TODO()
        is Expression.When -> TODO()
    }

    private fun checkNullPtrExpression(expression: Expression.NullPtr) {

    }

    private fun checkBinaryOperation(expression: Expression.BinaryOperation) {

    }

    private fun checkPropertyExpression(expression: Expression.Property) {
        val moduleProperty = ctx.resolver.resolveModuleProperty(expression)
        if (moduleProperty != null) {
            return
        }
        val lhsType = ctx.analyzer.typeOfExpression(expression.lhs)
        if (ctx.analyzer.resolvePropertyBinding(expression) == null) {
            error(Diagnostic.Kind.NoSuchProperty(lhsType, expression.property.name))
        }
    }

    private fun checkVarExpression(expression: Expression.Var) {
        val resolved = ctx.resolver.resolve(expression.name)
        if (resolved == null) {
            error(expression.name, Diagnostic.Kind.UnboundVariable(expression.name.name))
            return
        }
    }

    private fun checkCallExpression(expression: Expression.Call) {
        checkExpression(expression.callee)
        expression.args.forEach { checkExpression(it.expression) }
        expression.typeArgs?.let { args ->
            args.forEach { checkTypeAnnotation(it) }
        }

        val calleeType = ctx.analyzer.getCalleeType(expression)
        val fnTypeComponents = ctx.analyzer.getFunctionTypeComponents(calleeType)
        if (fnTypeComponents == null) {
            error(expression.callee, Diagnostic.Kind.TypeNotCallable(calleeType))
            return
        }

        val receiver = ctx.analyzer.getCallReceiver(expression)
        val expectedArgSize = if (receiver == null) fnTypeComponents.from.size else fnTypeComponents.from.size - 1
        if (expression.args.size > expectedArgSize) {
            error(expression.callee, Diagnostic.Kind.TooManyArgs(expectedArgSize))
        } else if (expression.args.size < expectedArgSize) {
            error(expression.callee, Diagnostic.Kind.MissingArgs(expectedArgSize))
        }

        if (expression.typeArgs != null) {
            val expectedTypeArgs = fnTypeComponents.typeParams?.size ?: 0
            if (expectedTypeArgs < expression.typeArgs.size) {
                error(expression.callee, Diagnostic.Kind.TooManyTypeArgs)
            }
        }
        val typeArgs = ctx.analyzer.getTypeArgs(expression)
        if (fnTypeComponents.traitRequirements != null) {
            val substitution = (fnTypeComponents.typeParams?: emptyList()).zip(typeArgs ?: emptyList())
                .map { (p, arg) -> p.binder.location to arg }
                .toMap()

            checkTraitInstances(
                expression,
                fnTypeComponents.traitRequirements.map { it.applySubstitution(substitution) }
            )
        }

        val fromTypes =
            if (receiver != null) {
                fnTypeComponents.from.drop(1)
            } else {
                fnTypeComponents.from
            }
        fromTypes.zip(expression.args).forEach { (expectedType, arg) ->
            val actualType = ctx.analyzer.typeOfExpression(arg.expression)
            if (!actualType.isAssignableTo(expectedType)) {
                error(arg, Diagnostic.Kind.TypeNotAssignable(destination = expectedType, source = actualType))
            }
        }

    }


    private fun checkTraitInstances(callNode: Expression, requiredInstances: List<TraitRequirement>) {
        for (requirement in requiredInstances) {
            if (!ctx.analyzer.isTraitRequirementSatisfied(callNode, requirement)) {
                error(callNode, Diagnostic.Kind.NoImplementationFound(
                    TraitRequirement(
                        requirement.traitRef,
                        requirement.arguments)
                ))
            }
        }
    }

    private fun checkFunctionSignature(signature: FunctionSignature) {
        if (ctx.resolver.getEnclosingExtensionDef(signature) != null) {
            checkTopLevelExpressionBinding(signature.name)

            if (signature.thisParamFlags == null) {
                error(signature.name, Diagnostic.Kind.MissingThisParam)
            }
        } else {
            if (signature.thisParamFlags != null) {
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

    private val topLevelExpressionBindings = mutableMapOf<Name, Binder>()
    private fun checkTopLevelExpressionBinding(binder: Binder) {
        if (binder.name in topLevelExpressionBindings) {
            error(binder, Diagnostic.Kind.DuplicateValueBinding(binder))
        } else {
            topLevelExpressionBindings[binder.name] = binder
        }
    }

    private val topLevelTypeBindings = mutableMapOf<Name, Binder>()
    private fun checkTopLevelTypeBinding(binder: Binder) {
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