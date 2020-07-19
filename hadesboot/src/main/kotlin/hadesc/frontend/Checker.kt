package hadesc.frontend

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.ir.BinaryOperator
import hadesc.ir.passes.TypeTransformer
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
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
    private val returnTypeStack = Stack<Type>()
    private val genericInstances = mutableMapOf<Long, Type>()

    fun resolvePropertyBinding(expression: Expression.Property): PropertyBinding? {

        val modulePropertyBinding = ctx.resolver.resolveModuleProperty(expression)
        if (modulePropertyBinding != null) {
            return PropertyBinding.Global(modulePropertyBinding)
        }

        val fieldBinding = resolveStructFieldBinding(expression)
        if (fieldBinding != null) {
            return fieldBinding
        }

        val globalExtensionFunctionBinding = resolveExtensionFunction(expression)
        if (globalExtensionFunctionBinding != null) {
            return globalExtensionFunctionBinding
        }

        val interfaceExtensionFunctionBinding = resolveInterfaceExtensionFunction(expression)
        if (interfaceExtensionFunctionBinding != null) {
            return interfaceExtensionFunctionBinding
        }

        error(expression.property, Diagnostic.Kind.NoSuchProperty(
            inferExpression(expression.lhs), expression.property.name))
        return null
    }

    private fun resolveInterfaceExtensionFunction(expression: Expression.Property): PropertyBinding? {
        val lhsType = inferExpression(expression.lhs)
        val impls = implsInScope(expression, lhsType)
        for (impl in impls) {
            val index = implPropertyIndex(impl, lhsType, expression)
            if (index != null) {
                return PropertyBinding.InterfaceExtensionFunction(
                    impl,
                    index
                )
            }
        }
        return null
    }

    private fun doesTypeSatisfyConstraint(location: SourceLocation, type: Type, constraint: Type.Constraint): Boolean {
        val impls = implsInScope(location, type)
        require(constraint.args.isEmpty()) { TODO() }
        return impls.find {
            constraint.interfaceName == resolveInterfaceName(it.interfaceRef)
        } != null
    }

    private fun resolveInterfaceName(interfaceRef: InterfaceRef): QualifiedName? {
        return resolveInterfaceDeclaration(interfaceRef)?.let {
            ctx.resolver.qualifiedInterfaceName(it)
        }
    }

    private fun implPropertyIndex(
        impl: ImplementationBinding,
        lhsType: Type,
        expression: Expression.Property
    ): Int? {
        val interfaceRef = impl.interfaceRef
        val interfaceDecl = ctx.resolver.resolveDeclaration(interfaceRef.path)
        if (interfaceDecl !is Declaration.Interface) {
            return null
        }
        interfaceDecl.members.forEachIndexed { index, member ->
            when (member) {
                is Declaration.Interface.Member.FunctionSignature -> {
                    if (member.signature.name.identifier.name == expression.property.name) {
                        return index
                    }
                }
            }
        }
        return null
    }

    private fun implsInScope(node: HasLocation, lhsType: Type): List<ImplementationBinding> = buildList {
        when (lhsType) {
            is Type.ParamRef -> {
                val functionDef = ctx.resolver.getEnclosingFunction(node)
                val functionDefParam =
                    functionDef?.typeParams?.find { lhsType.name == it.binder }
                if (functionDef is Declaration.FunctionDef && functionDefParam != null) {
                    val bound = functionDefParam.bound
                    val paramIndex = functionDef.typeParams?.indexOf(functionDefParam) ?: requireUnreachable()
                    if (bound != null) {
                        add(ImplementationBinding.TypeBound(
                            bound,
                            functionDef,
                            paramIndex))
                    } else if (functionDef.signature.whereClause != null) {
                        addAll(functionDef.signature.whereClause.constraints.filter {
                            it.param.name == functionDefParam.binder.identifier.name
                        }.map {
                            ImplementationBinding.TypeBound(
                                it.interfaceRef,
                                functionDef,
                                paramIndex
                            )
                        })
                    }
                }
                val implDef = ctx.resolver.getEnclosingImplementationDef(lhsType.name)
                if (implDef != null) {
                    val param =
                        implDef.typeParams?.find { lhsType.name == it.binder }
                    val bound = param?.bound
                    val paramIndex = implDef.typeParams?.indexOf(param) ?: return@buildList
                    if (bound != null) {
                        add(ImplementationBinding.ImplParamTypeBound(
                            bound,
                            implDef,
                            paramIndex))
                    } else if (implDef.whereClause != null) {
                        addAll(implDef.whereClause.constraints.filter {
                            it.param.name == param?.binder?.identifier?.name
                        }.map {
                            ImplementationBinding.ImplParamTypeBound(
                                it.interfaceRef,
                                implDef,
                                paramIndex
                            )
                        })
                    }
                }

            }
            is Type.Application -> {
                if (lhsType.callee is Type.Constructor) {
                    addAll(implsInScopeForTypeConstructor(node, lhsType.callee, lhsType.args))
                }
            }
            is Type.Constructor -> {
                addAll(implsInScopeForTypeConstructor(node, lhsType, null))
            }
            is Type.Ptr -> {
                addAll(implsInScope(node, lhsType.to))
            }
            else -> {
                addAll(implsInScopeForType(node, lhsType))
            }
        }
    }

    private fun implsInScopeForType(node: HasLocation, lhsType: Type): Collection<ImplementationBinding> = buildList {
        for (implementation in ctx.resolver.implementationsInScope(node)) {
            val substitution = instantiateSubstitution(
                implementation.typeParams?.map { Type.Param(it.binder) },
                constraintsOf(implementation.typeParams, implementation.whereClause),
                node.location
            )
            val forType = annotationToType(implementation.forType).applySubstitution(substitution)

            if (isTypeAssignableTo(source = lhsType, destination = forType)) {
                add(ImplementationBinding.GlobalImpl(implementation))
            }
        }
    }

    private fun implsInScopeForTypeConstructor(
        node: HasLocation,
        lhsType: Type.Constructor,
        typeArgs: List<Type>?
    ): Collection<ImplementationBinding> = buildList {
        val appliedLhsType = if (typeArgs != null)
            Type.Application(lhsType, typeArgs)
        else lhsType
        return implsInScopeForType(node, appliedLhsType)
    }

    private fun constraintsOf(typeParams: List<TypeParam>?, whereClause: WhereClause?): List<Type.Constraint> = buildList {
        typeParams?.forEach { typeParam ->
            if (typeParam.bound != null) {
                val declaration = resolveInterfaceDeclaration(typeParam.bound)
                if (declaration != null) {
                    add(Type.Constraint(
                        interfaceName = ctx.resolver.qualifiedInterfaceName(declaration),
                        param = Type.Param(typeParam.binder),
                        args = typeParam.bound.typeArgs?.map { annotationToType(it) } ?: emptyList()
                    ))
                }
            }
        }
        whereClause?.constraints?.forEach { constraint ->
            val declaration = resolveInterfaceDeclaration(constraint.interfaceRef)
            val typeParam = typeParams?.find {  constraint.param.name == it.binder.identifier.name }
            if (declaration != null && typeParam != null) {

                add(Type.Constraint(
                    interfaceName = ctx.resolver.qualifiedInterfaceName(declaration),
                    param = Type.Param(typeParam.binder),
                    args = constraint.interfaceRef.typeArgs?.map { annotationToType(it) } ?: emptyList()
                ))
            }
        }
    }

    private fun resolveInterfaceDeclaration(interfaceRef: InterfaceRef): Declaration.Interface? {
        val declaration = ctx.resolver.resolveDeclaration(interfaceRef.path)
        return if (declaration is Declaration.Interface) {
            declaration
        } else {
            null
        }
    }


    private fun resolveStructFieldBinding(expression: Expression.Property): PropertyBinding? {
        val lhsType = inferExpression(expression.lhs)
        val structDecl = getStructDeclOfType(lhsType)
        return if (structDecl == null) null else {
            val index = structDecl.members.indexOfFirst {
                it is Declaration.Struct.Member.Field
                        && it.binder.identifier.name == expression.property.name
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
                        memberIndex = index
                )
            } else null
        }

    }

    private fun getStructDeclOfType(lhsType: Type): Declaration.Struct? {
        return when (lhsType) {
            is Type.Constructor -> {
                val decl = ctx.resolver.resolveDeclaration(lhsType.name)
                require(decl is Declaration.Struct)
                decl
            }
            is Type.Application -> {
                getStructDeclOfType(lhsType.callee)
            }
            else -> null
        }
    }

    private fun resolveExtensionFunction(expression: Expression.Property): PropertyBinding? {
        val lhsType = inferExpression(expression.lhs)
        for (functionDef in ctx.resolver.extensionDefsInScope(expression, expression.property)) {
            if (isExtensionFor(functionDef, lhsType)) {
                return PropertyBinding.GlobalExtensionFunction(functionDef)
            }
        }
        return null
    }

    private fun isExtensionFor(functionDef: Declaration.FunctionDef, lhsType: Type): Boolean {
        require(functionDef.thisParam != null)
        val thisType = annotationToType(functionDef.thisParam.annotation)

        val typeArgs = if (lhsType is Type.Application) {
            lhsType.args
        } else {
            emptyList()
        }

        val substitution = functionDef.typeParams?.zip(typeArgs)
                ?.map { it.first.binder.location to it.second }
                ?.toMap()
                ?: emptyMap()
        return isTypeAssignableTo(lhsType, thisType.applySubstitution(substitution))
    }

    private fun equateTypes(source: Type, destination: Type) {
        // isTypeAssignable is side effectful in the sense that it instantiates
        // Type.GenericInstance types for matching source and destination types
        isTypeAssignableTo(source, destination)
    }

    private fun isTypeAssignableTo(source: Type, destination: Type): Boolean {
        if (source == destination) {
            return true
        }
        return when {
            destination is Type.GenericInstance -> {
                val existing = genericInstances[destination.id]
                when {
                    existing != null -> {
                        isTypeAssignableTo(source, destination = existing)
                    }
                    destination.constraints.all { doesTypeSatisfyConstraint(
                        destination.intantiationLocation,
                        reduceGenericInstances(source),
                        it
                    ) } -> {
                        genericInstances[destination.id] = source
                        true
                    }
                    else -> {
                        false
                    }
                }
            }
            source is Type.GenericInstance -> {
                val existing = genericInstances[source.id]
                if (existing != null) {
                    isTypeAssignableTo(existing, destination = destination)
                } else {
                    genericInstances[source.id] = destination
                    true
                }
            }
            destination is Type.Application && source is Type.Application -> {
                isTypeAssignableTo(source.callee, destination.callee)
                        && source.args.size == destination.args.size
                        && source.args.zip(destination.args).all {
                            isTypeAssignableTo(it.first, it.second)
                        }
            }

            destination is Type.ParamRef && source is Type.ParamRef -> {
                destination.name.identifier.name == source.name.identifier.name
            }
            destination is Type.Ptr && source is Type.Ptr -> {
                val ptrTypeAssignable = isTypeAssignableTo(source.to, destination.to)
                if (destination.isMutable && !source.isMutable) {
                    false
                } else {
                    ptrTypeAssignable
                }

            }
            destination is Type.Application && source is Type.Application -> {
                isTypeAssignableTo(source = source.callee, destination = destination.callee)
                        && source.args.zip(destination.args).all {
                    isTypeAssignableTo(source = it.first, destination = it.second)
                }
            }
            destination is Type.Constructor && source is Type.Constructor -> {
                destination.name == source.name
            }
            else -> false
        }
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
        when (declaration) {
            is Declaration.Error -> {}
            is Declaration.ImportAs -> checkImportAsDeclaration(declaration)
            is Declaration.FunctionDef -> checkFunctionDefDeclaration(declaration)
            is Declaration.ConstDefinition -> TODO()
            is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
            is Declaration.Struct -> checkStructDeclaration(declaration)
            is Declaration.Interface -> checkInterfaceDeclaration(declaration)
            is Declaration.Implementation -> checkImplementationDeclaration(declaration)
            is Declaration.Enum -> TODO()
            is Declaration.TypeAlias -> checkTypeAliasDeclaration(declaration)
        }
    }

    private fun checkImplementationDeclaration(declaration: Declaration.Implementation) {
        declaration.typeParams?.forEach { checkTypeParam(it) }
        checkInterfaceRef(declaration.interfaceRef)
        declaration.whereClause?.let { checkWhereClause(it) }
        fun checkMember(member: Declaration.Implementation.Member) = when(member) {
            is Declaration.Implementation.Member.FunctionDef -> {
                checkFunctionDefDeclaration(member.functionDef)
            }
        }
        for (member in declaration.members) {
            checkMember(member)
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
        param.bound?.let { checkInterfaceRef(it) }
    }

    private fun checkInterfaceRef(interfaceRef: InterfaceRef) {
        val declaration = ctx.resolver.resolveDeclaration(interfaceRef.path)
        if (declaration == null) {
            error(interfaceRef, Diagnostic.Kind.UnboundInterface)
        } else if (declaration !is Declaration.Interface){
            error(interfaceRef, Diagnostic.Kind.NotAnInterface)
        }
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

    private fun checkInterfaceDeclaration(declaration: Declaration.Interface) {
        checkInterfaceNameBinder(declaration.name)
        declaration.typeParams?.forEach {
            checkTypeParam(it)
        }

        fun checkMethodReceiver(receiver: ThisParam) {
            val thisType = annotationToType(receiver.annotation)
            if (thisType !is Type.ThisRef) {
                if (thisType is Type.Ptr) {
                    if (thisType.to !is Type.ThisRef) {
                        error(receiver.annotation, Diagnostic.Kind.InterfaceMethodReceiverMustBeThisOrThisPtr)
                    }
                } else {
                    error(receiver.annotation, Diagnostic.Kind.InterfaceMethodReceiverMustBeThisOrThisPtr)
                }
            }
        }

        fun checkMember(member: Declaration.Interface.Member) = when (member) {
            is Declaration.Interface.Member.FunctionSignature -> {
                checkFunctionSignature(member.signature)
                if (member.signature.typeParams != null) {
                    error(member.signature, Diagnostic.Kind.TypeParametersNotAllowedInInterfaceMethods)
                }
                member.signature.typeParams?.forEach {
                    checkTypeParam(it)
                }

                if (member.signature.thisParam == null) {
                    error(member.signature, Diagnostic.Kind.InterfaceMemberMustBeAnExtensionMethod)
                }

                member.signature.thisParam?.let {
                    checkMethodReceiver(member.signature.thisParam)
                }
            }
        }

        for (member in declaration.members) {
            checkMember(member)
        }

    }

    private fun checkWhereClause(clause: WhereClause) {
        for (constraint in clause.constraints) {
            checkInterfaceRef(constraint.interfaceRef)
            when(ctx.resolver.resolveTypeVariable(constraint.param)) {
                null -> error(constraint.param, Diagnostic.Kind.UnboundType(constraint.param.name))
                !is TypeBinding.TypeParam -> {
                    error(constraint.param, Diagnostic.Kind.WhereClauseMustReferToATypeParam)
                }
                else -> {}
            }
        }
    }

    private fun checkInterfaceNameBinder(
        @Suppress("UNUSED_PARAMETER")
        name: Binder) {
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
        annotationToType(signature.returnType)
        signature.whereClause?.let { checkWhereClause(it) }
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
        TODO()
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
                error(statement.name, Diagnostic.Kind.UnboundVariable)
            }
            else -> {
                // TODO: Show a more helpful diagnostic here
                error(statement.name, Diagnostic.Kind.NotAnAddressableValue)
            }
        }
    }

    private fun checkMemberAssignment(statement: Statement.MemberAssignment) {
        val lhsType = inferExpression(statement.lhs)
        val rhsType = inferExpression(statement.value)
        val field = resolvePropertyBinding(statement.lhs)
        if (field !is PropertyBinding.StructField) {
            error(statement.lhs.property, Diagnostic.Kind.NotAStructField)
            return
        }

        if (!field.member.isMutable) {
            error(statement.lhs.property, Diagnostic.Kind.StructFieldNotMutable)
            return
        }

        checkAssignability(statement.value, destination = lhsType, source = rhsType)
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
            checkExpression(statement.value, returnType)
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

    private fun checkExpression(expression: Expression, expectedType: Type): Type {
        val type = when (expression) {
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
            else -> {
                val inferredType = inferExpression(expression)
                checkAssignability(expression, source = inferredType, destination = expectedType)
                expectedType
            }
        }
        typeOfExpressionCache[expression] = type
        return type
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
        else -> false
    }

    private fun inferExpression(expression: Expression): Type = typeOfExpressionCache.getOrPut(expression) {
        when (expression) {
            is Expression.Error -> Type.Error
            is Expression.Var -> inferVarExpresion(expression)
            is Expression.Call -> inferCallExpression(expression)
            is Expression.Property -> inferPropertyExpression(expression)
            is Expression.ByteString -> Type.Ptr(Type.Byte, isMutable = false)
            is Expression.BoolLiteral -> Type.Bool
            is Expression.This -> inferThisExpression(expression)
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
        }
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
                error(path, Diagnostic.Kind.UnboundTypeName(path))
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
            inferExpression(expression.rhs)
            error(expression.location, Diagnostic.Kind.OperatorNotApplicable(expression.operator))
            Type.Error
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

    private fun inferThisExpression(expression: Expression.This): Type {
        val thisBinding = ctx.resolver.resolveThisParam(expression)
        return if (thisBinding == null) {
            Type.Error
        } else {
            return annotationToType(thisBinding.annotation)
        }
    }

    private fun inferNotExpression(expression: Expression.Not): Type {
        return checkExpression(expression.expression, Type.Bool)
    }

    private fun inferPropertyExpression(expression: Expression.Property): Type =
            when (val binding = resolvePropertyBinding(expression)) {
                is PropertyBinding.Global -> typeOfGlobalPropertyBinding(binding)
                is PropertyBinding.StructField -> typeOfStructFieldProperty(expression, binding)
                is PropertyBinding.StructFieldPointer -> TODO()
                is PropertyBinding.GlobalExtensionFunction -> typeOfGlobalExtensionMethod(expression, binding)
                is PropertyBinding.InterfaceExtensionFunction -> typeOfInterfaceExtensionMethod(binding)
                null -> Type.Error
            }

    private fun typeOfInterfaceExtensionFunction(expression: Expression.Property, binding: PropertyBinding.InterfaceExtensionFunction): Type {
        val interfaceRef = binding.implementationBinding.interfaceRef
        val interfaceDeclaration = resolveInterfaceDeclaration(interfaceRef) ?: return Type.Error
        val lhsType = inferExpression(expression.lhs)
        return interfaceMemberPropertyType(lhsType, interfaceDeclaration, binding.memberIndex)
    }

    private fun interfaceMemberPropertyType(
        lhsType: Type,
        interfaceDeclaration: Declaration.Interface,
        memberIndex: Int
    ): Type {
        val method = interfaceDeclaration.members[memberIndex]
        if (method !is Declaration.Interface.Member.FunctionSignature) {
            return Type.Error
        }
        require(method.signature.typeParams == null)

        return substituteThisParam(Type.Function(
            receiver = null,
            constraints = emptyList(),
            from = method.signature.params.map { param -> param.annotation?.let { annotationToType(it) } ?: Type.Error },
            to = annotationToType(method.signature.returnType)
        ), with = lhsType)
    }

    private fun substituteThisParam(type: Type, with: Type): Type {
        return object : TypeTransformer {
            override fun lowerThisRefType(type: Type.ThisRef): Type {
                return with
            }
        }.lowerType(type)
    }

    private fun typeOfStructFieldProperty(
            expression: Expression.Property,
            binding: PropertyBinding.StructField): Type {
        val lhsType = inferExpression(expression.lhs)
        val typeArgs = if (lhsType is Type.Application)  {
            lhsType.args
        } else emptyList()
        val substitution = binding.structDecl.typeParams?.zip(typeArgs)
                ?.map { it.first.binder.location to it.second }
                ?.toMap()
                ?: emptyMap()
        val genericMemberType = annotationToType(binding.member.typeAnnotation)
        val memberType = genericMemberType.applySubstitution(substitution)
        equateTypes(lhsType, memberType)
        return memberType
    }

    private fun typeOfGlobalExtensionMethod(expression: Expression.Property, binding: PropertyBinding.GlobalExtensionFunction): Type {
        val lhsType = inferExpression(expression.lhs)
        val functionType = typeOfGlobalFunctionRef(binding.def)
        val functionTypeComponents = getFunctionTypeComponents(functionType)
        requireNotNull(functionTypeComponents)
        requireNotNull(functionTypeComponents.receiverType)
        val substitution = instantiateSubstitution(
            functionTypeComponents.typeParams,
            functionTypeComponents.constraints,
            expression.location
        )
        equateTypes(lhsType, functionTypeComponents.receiverType.applySubstitution(substitution))
        return Type.Function(
                receiver = null,
                from = functionTypeComponents.from.map { it.applySubstitution(substitution) },
                to = functionTypeComponents.to.applySubstitution(substitution)
        )
    }

    private fun typeOfGlobalPropertyBinding(
            binding: PropertyBinding.Global): Type {
        return typeOfBinding(binding.binding)
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
        is Binding.ValBinding -> typeOfValRef(binding)
        is Binding.Struct -> typeOfStructValueRef(binding)
        is Binding.GlobalConst -> TODO()
        is Binding.EnumCaseConstructor -> TODO()
        is Binding.Pattern -> TODO()
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
        val functionType = Type.Function(receiver = null, from = fieldTypes, to = instanceType)
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
                receiver = declaration.thisParam?.let { annotationToType(it.annotation) },
                constraints = constraintsOf(declaration.typeParams, declaration.signature.whereClause)
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
            val receiverType: Type?,
            val from: List<Type>,
            val to: Type,
            val typeParams: List<Type.Param>?,
            val constraints: List<Type.Constraint>
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
        val receiverArg = getReceiverArg(callNode)
        val functionType = getFunctionTypeComponents(calleeType)
        if (receiverArg != null) {
            inferExpression(receiverArg)
        }
        return if (functionType == null) {
            for (arg in args) {
                inferExpression(arg.expression)
            }
            error(callNode, Diagnostic.Kind.TypeNotCallable(calleeType))
            Type.Error
        } else {
            val substitution = instantiateSubstitution(
                functionType.typeParams,
                functionType.constraints,
                callNode.location
            )
            if (receiverArg != null) {
                require(functionType.receiverType != null)
                val inferredType = reduceGenericInstances(inferExpression(receiverArg))

                val thisType = if (
                    functionType.receiverType is Type.Ptr &&
                    functionType.receiverType.to is Type.ThisRef &&
                    inferredType is Type.Ptr) {
                    inferredType.to
                } else  inferredType

                checkAssignability(
                    receiverArg,
                    source = inferredType,
                    destination = substituteThisParam(
                        functionType.receiverType.applySubstitution(substitution),
                        with = thisType
                    )
                )
            }
            checkCallArgs(functionType, callNode, explicitTypeArgs, args, substitution)
            if (expectedReturnType != null) {
                val source = functionType.to.applySubstitution(substitution)
                checkAssignability(
                        callNode,
                        source = source,
                        destination = expectedReturnType
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

    private fun getReceiverArg(callNode: Expression): Expression? = when(callNode) {
        is Expression.Call -> when (callNode.callee) {
            is Expression.Property -> when (resolvePropertyBinding(callNode.callee)) {
                is PropertyBinding.InterfaceExtensionFunction ->
                    callNode.callee.lhs
                is PropertyBinding.GlobalExtensionFunction ->
                    callNode.callee.lhs
                else -> null
            }
            else -> null
        }
        is Expression.New -> null
        else -> null
    }

    private fun getCalleeType(callNode: Expression): Type = when(callNode) {
        is Expression.Call -> when (callNode.callee) {
            is Expression.Property -> when (val binding = resolvePropertyBinding(callNode.callee)) {
                is PropertyBinding.InterfaceExtensionFunction -> {
                    inferExpression(callNode.callee)
                    typeOfInterfaceExtensionMethod(binding)
                }
                is PropertyBinding.GlobalExtensionFunction -> {
                    inferExpression(callNode.callee)
                    typeOfGlobalFunctionRef(binding.def)
                }
                else -> inferExpression(callNode.callee)
            }
            else -> inferExpression(callNode.callee)
        }
        is Expression.New -> checkConstructorFunction(callNode.qualifiedPath)
        else -> Type.Error
    }

    private fun typeOfInterfaceExtensionMethod(binding: PropertyBinding.InterfaceExtensionFunction): Type {
        val interfaceDef = resolveInterfaceDeclaration(binding.implementationBinding.interfaceRef) ?: requireUnreachable()
        val member = interfaceDef.members[binding.memberIndex]

        require(member is Declaration.Interface.Member.FunctionSignature)
        val signature = member.signature
        require(signature.typeParams == null)
        require(signature.whereClause == null)
        val functionType = Type.Function(
            receiver = member.signature.thisParam?.annotation?.let { annotationToType(it) },
            constraints = constraintsOf(interfaceDef.typeParams, null),
            from = signature.params.map { it.annotation?.let { annot -> annotationToType(annot) } ?: Type.Error },
            to = annotationToType(signature.returnType)
        )

        return if (interfaceDef.typeParams != null)
            Type.TypeFunction(
                params = interfaceDef.typeParams.map { Type.Param(it.binder) },
                body = functionType
            )
        else functionType
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
        constraints: List<Type.Constraint>,
        instantiationLocation: SourceLocation
    ): Substitution {
        return typeParams
                ?.map { it.binder.location to makeGenericInstance(
                    it.binder,
                    constraints.filter { constraint -> constraint.param.binder.location == it.binder.location },
                    instantiationLocation = instantiationLocation
                ) }
                ?.toMap()
                ?: emptyMap()
    }

    private fun checkCallArgs(
            functionType: FunctionTypeComponents,
            callNode: HasLocation,
            typeArgs: List<Type>?,
            args: List<Arg>,
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
            checkExpression(arg.expression, expectedType)
        }
        for (arg in args.drop(length)) {
            inferExpression(arg.expression)
        }

        checkArgumentLength(callNode, args, functionType)
    }

    private fun checkArgumentLength(
            callNode: HasLocation,
            args: List<Arg>,
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
                            receiverType = type.receiver,
                            from = type.from,
                            to = type.to,
                            typeParams = typeParams,
                            constraints = type.constraints
                    )
                is Type.TypeFunction -> {
                    return recurse(type.body, type.params)
                }
                is Type.GenericInstance -> recurse(reduceGenericInstances(type), null)
                is Type.Error -> {
                    return FunctionTypeComponents(
                        receiverType = Type.Error,
                        from = emptyList(),
                        to = Type.Error,
                        typeParams = null,
                        constraints = emptyList()
                    )
                }
                else -> null
            }
        }
        return recurse(type, null)

    }

    private var makeGenericInstanceId = 0L
    private fun makeGenericInstance(
        binder: Binder,
        constraints: List<Type.Constraint>,
        instantiationLocation: SourceLocation
    ): Type.GenericInstance {
        val id = makeGenericInstanceId
        makeGenericInstanceId++
        return Type.GenericInstance(
            binder,
            id = id,
            constraints = constraints,
            intantiationLocation = instantiationLocation)
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
            is TypeAnnotation.Function -> TODO()
            is TypeAnnotation.This -> {
                val interfaceDecl = ctx.resolver.getEnclosingInterfaceDecl(annotation)
                if (interfaceDecl == null) {
                    error(annotation, Diagnostic.Kind.UnboundThisType)
                    Type.Error
                } else {
                    Type.ThisRef(interfaceDecl.location)
                }
            }
            is TypeAnnotation.Union -> unionAnnotationToType(annotation)
        }
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
                val existing = genericInstances[type.id]
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
        (op.NOT_EQUALS to Type.Size) to (Type.Size to Type.Bool)
)

