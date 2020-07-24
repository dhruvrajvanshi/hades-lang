package hadesc.frontend

import hadesc.Name
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
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

        val elementPointerBinding = resolveElementPointerBinding(expression)
        if (elementPointerBinding != null) {
            return elementPointerBinding
        }

        error(expression.property, Diagnostic.Kind.NoSuchProperty(
            inferExpression(expression.lhs), expression.property.name))
        return null
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
                require(decl is Declaration.Struct)
                decl
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

    private fun isTypeAssignableTo(source: Type, destination: Type): Boolean {
        if (source == destination) {
            return true
        }
        return when {
            destination is Type.GenericInstance -> {
                val existing = genericInstances[destination.id]
                if (existing != null) {
                    isTypeAssignableTo(source, destination = existing)
                } else {
                    genericInstances[destination.id] = source
                    true
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
            destination is Type.Function && source is Type.Function -> {
                destination.from.size == source.from.size
                        && isTypeAssignableTo(source = source.to, destination = destination.to)
                        && source.from.zip(destination.from).all { (sourceParam, destParam) ->
                            isTypeAssignableTo(source = destParam, destination = sourceParam)
                        }
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
            is Declaration.Enum -> TODO()
            is Declaration.TypeAlias -> checkTypeAliasDeclaration(declaration)
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
        })
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

    private fun inferNotExpression(expression: Expression.Not): Type {
        return checkExpression(expression.expression, Type.Bool)
    }

    private fun inferPropertyExpression(expression: Expression.Property): Type =
            when (val binding = resolvePropertyBinding(expression)) {
                is PropertyBinding.Global -> typeOfGlobalPropertyBinding(binding)
                is PropertyBinding.StructField -> binding.type
                is PropertyBinding.StructFieldPointer -> binding.type
                null -> Type.Error
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
        val functionType = Type.Function(from = fieldTypes, to = instanceType)
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
                        to = annotationToType(declaration.returnType)
                ),
                isMutable = false
        )
    }

    private fun typeOfGlobalFunctionRef(declaration: Declaration.FunctionDef): Type {
        val functionType = Type.Function(
                from = declaration.params.map { param ->
                    param.annotation?.let { annotationToType(it) } ?: Type.Error },
                to = annotationToType(declaration.signature.returnType)
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
            val typeParams: List<Type.Param>?
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

    private fun getCalleeType(callNode: Expression): Type = when(callNode) {
        is Expression.Call -> when (callNode.callee) {
            is Expression.Property -> when (val binding = resolvePropertyBinding(callNode.callee)) {
                else -> inferExpression(callNode.callee)
            }
            else -> inferExpression(callNode.callee)
        }
        is Expression.New -> checkConstructorFunction(callNode.qualifiedPath)
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
                    it.binder,
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
                            from = type.from,
                            to = type.to,
                            typeParams = typeParams
                    )
                is Type.TypeFunction -> {
                    return recurse(type.body, type.params)
                }
                is Type.GenericInstance -> recurse(reduceGenericInstances(type), null)
                is Type.Error -> {
                    return FunctionTypeComponents(
                        from = emptyList(),
                        to = Type.Error,
                        typeParams = null
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
        instantiationLocation: SourceLocation
    ): Type.GenericInstance {
        val id = makeGenericInstanceId
        makeGenericInstanceId++
        return Type.GenericInstance(
            binder,
            id = id,
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
            is TypeAnnotation.Function -> functionAnnotationToType(annotation)
            is TypeAnnotation.Union -> unionAnnotationToType(annotation)
        }
    }

    private fun functionAnnotationToType(annotation: TypeAnnotation.Function): Type {
        return Type.Function(
            from = annotation.from.map { annotationToType(it) },
            to = annotationToType(annotation.to)
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

