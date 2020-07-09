package hadesc.typer

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
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
class Typer(
        private val ctx: Context
) {
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
        return null
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
                isTypeAssignableTo(lhsType, fieldType, Variance.INVARIANCE)
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
        return isTypeAssignableTo(lhsType, thisType.applySubstitution(substitution), Variance.INVARIANCE)
    }

    private fun collapseTypeFunctionParams(type: Type, typeParams: List<TypeParam>?): Type {
        return object : TypeTransformer {
            override fun lowerTypeFunction(type: Type.TypeFunction): Type {
                return if (typeParams == null) {
                    super.lowerTypeFunction(type)
                } else {
                    val firstParamLocation = type.params[0].binder.location
                    if (typeParams[0].location == firstParamLocation) {
                        require(typeParams.map { it.location } == type.params.map { it.binder.location })
                        lowerType(type.body)
                    } else {
                        super.lowerTypeFunction(type)
                    }
                }
            }
        }.lowerType(type)

    }

    private fun checkAssignability(source: Type, destination: Type) {
        isTypeAssignableTo(source, destination)
    }
    fun isTypeAssignableTo(source: Type, destination: Type, variance: Variance = Variance.INVARIANCE): Boolean {
        if (source == destination) {
            return true
        }
        return when {
            destination is Type.GenericInstance -> {
                val existing = genericInstances[destination.id]
                if (existing != null) {
                    isTypeAssignableTo(source, destination = existing, variance = Variance.INVARIANCE)
                } else {
                    genericInstances[destination.id] = source
                    true
                }
            }
            destination is Type.Application && source is Type.Application -> {
                isTypeAssignableTo(source.callee, destination.callee, Variance.INVARIANCE)
                        && source.args.size == destination.args.size
                        && source.args.zip(destination.args).all {
                            isTypeAssignableTo(it.first, it.second, Variance.INVARIANCE)
                        }
            }

            destination is Type.ParamRef && source is Type.ParamRef -> {
                destination.name.identifier.name == source.name.identifier.name
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
        visitFunction(def)
        return requireNotNull(typeOfExpressionCache[expression])

    }

    private fun visitFunction(def: Declaration.FunctionDef) {
        val returnType = annotationToType(def.signature.returnType)
        returnTypeStack.push(returnType)
        visitBlock(def.body)
        returnTypeStack.pop()

    }

    private fun visitBlock(block: Block) {
        for (member in block.members) {
            visitBlockMember(member)
        }
    }

    private fun visitBlockMember(member: Block.Member): Unit = when(member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> visitStatement(member.statement)
    }

    private fun visitStatement(statement: Statement): Unit = when(statement) {
        is Statement.Return -> visitReturnStatement(statement)
        is Statement.Val -> visitValStatement(statement)
        is Statement.While -> visitWhileStatement(statement)
        is Statement.If -> visitIfStatement(statement)
        is Statement.LocalAssignment -> TODO()
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Defer -> TODO()
        is Statement.Error -> TODO()
    }

    private fun visitWhileStatement(statement: Statement.While) {
        checkExpression(statement.condition, Type.Bool)
        visitBlock(statement.body)
    }

    private fun visitIfStatement(statement: Statement.If) {
        checkExpression(statement.condition, Type.Bool)
        visitBlock(statement.ifTrue)
        statement.ifFalse?.let { visitBlock(it) }
    }

    private fun visitReturnStatement(statement: Statement.Return) {
        if (statement.value != null) {
            val returnType = returnTypeStack.peek()
            checkExpression(statement.value, returnType)
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

    private fun checkExpression(expression: Expression, expectedType: Type): Type = when(expression) {
        is Expression.IntLiteral -> {
            if (isIntLiteralAssignable(expectedType)) {
                typeOfExpressionCache[expression] = expectedType
                expectedType
            } else {
                inferExpression(expression)
            }
        }
        else -> {
            val inferredType = inferExpression(expression)
            isTypeAssignableTo(inferredType, expectedType, Variance.INVARIANCE)
            expectedType
        }
    }

    private fun isIntLiteralAssignable(type: Type): Boolean = when(type) {
        is Type.Size,
        is Type.CInt -> true
        else -> false
    }

    private fun inferExpression(expression: Expression): Type {
        val type = when (expression) {
            is Expression.Error -> Type.Error
            is Expression.Var -> inferVarExpresion(expression)
            is Expression.Call -> inferCallExpression(expression)
            is Expression.Property -> inferPropertyExpression(expression)
            is Expression.ByteString -> Type.Ptr(Type.Byte, isMutable = false)
            is Expression.BoolLiteral -> Type.Bool
            is Expression.This -> inferThisExpression(expression)
            is Expression.NullPtr -> Type.Error
            is Expression.IntLiteral -> inferIntLiteral(expression)
            is Expression.Not -> inferNotExpression(expression)
            is Expression.BinaryOperation -> TODO()
            is Expression.SizeOf -> TODO()
            is Expression.AddressOf -> TODO()
            is Expression.AddressOfMut -> TODO()
            is Expression.Deref -> TODO()
            is Expression.PointerCast -> TODO()
            is Expression.If -> inferIfExpression(expression)
            is Expression.TypeApplication -> TODO()
            is Expression.Match -> TODO()
            is Expression.New -> TODO()
        }

        typeOfExpressionCache[expression] = type
        return type

    }

    private fun inferIntLiteral(expression: Expression.IntLiteral): Type {
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
                is PropertyBinding.InterfaceExtensionFunction -> TODO()
                null -> Type.Error
            }

    private fun typeOfStructFieldProperty(
            expression: Expression.Property,
            binding: PropertyBinding.StructField): Type {
        val lhsType = typeOfExpression(expression.lhs)
        val typeArgs = if (lhsType is Type.Application)  {
            lhsType.args
        } else emptyList()
        val substitution = binding.structDecl.typeParams?.zip(typeArgs)
                ?.map { it.first.binder.location to it.second }
                ?.toMap()
                ?: emptyMap()
        val genericMemberType = annotationToType(binding.member.typeAnnotation)
        val memberType = genericMemberType.applySubstitution(substitution)
        checkAssignability(lhsType, memberType)
        return memberType
    }

    private fun typeOfGlobalExtensionMethod(expression: Expression.Property, binding: PropertyBinding.GlobalExtensionFunction): Type {
        val lhsType = typeOfExpression(expression.lhs)
        val functionType = typeOfGlobalFunctionRef(binding.def)
        val functionTypeComponents = getFunctionTypeComponents(functionType)
        requireNotNull(functionTypeComponents)
        requireNotNull(functionTypeComponents.receiverType)
        val substitution = functionTypeComponents.typeParams
                ?.map { it.binder.location to makeGenericInstance(it.binder) }
                ?.toMap()
                ?: emptyMap()
        checkAssignability(lhsType, functionTypeComponents.receiverType.applySubstitution(substitution))
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
            typeOfExpression(binding.statement.rhs)
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
                receiver = declaration.thisParam?.let { annotationToType(it.annotation) }
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
            val typeParams: List<Type.Param>?
    )
    private fun inferCallExpression(expression: Expression.Call): Type {
        return inferCallLikeExpression(inferExpression(expression.callee), expression.args)
    }

    private fun inferCallLikeExpression(
            calleeType: Type,
            args: List<Arg>
    ): Type {
        val functionType = getFunctionTypeComponents(calleeType)
        return if (functionType == null) {
            for (arg in args) {
                inferExpression(arg.expression)
            }
            Type.Error
        } else {
            val substitution = functionType.typeParams
                    ?.map { it.binder.location to makeGenericInstance(it.binder) }
                    ?.toMap()
                    ?: emptyMap()
            checkCallArgs(functionType, args, substitution)
            reduceGenericInstances(functionType.to.applySubstitution(substitution))
        }
    }

    private fun checkCallArgs(functionType: FunctionTypeComponents, args: List<Arg>, substitution: Substitution) {
        val length = min(functionType.from.size, args.size)
        for (i in 0 until length) {
            val expectedType = functionType.from[i].applySubstitution(substitution)
            val arg = args[i]
            checkExpression(arg.expression, expectedType)
        }
        for (arg in args.drop(length)) {
            inferExpression(arg.expression)
        }
    }

    private fun getFunctionTypeComponents(type: Type, typeParams: List<Type.Param>? = null): FunctionTypeComponents? {
        return when (type) {
            is Type.Ptr -> getFunctionTypeComponents(type.to)
            is Type.Function ->
                FunctionTypeComponents(
                        receiverType = type.receiver,
                        from = type.from,
                        to = type.to,
                        typeParams = typeParams
                )
            is Type.TypeFunction -> {
                return getFunctionTypeComponents(type.body, type.params)
            }
            is Type.GenericInstance -> getFunctionTypeComponents(reduceGenericInstances(type))
            else -> null
        }
    }

    private var makeGenericInstanceId = 0L
    private fun makeGenericInstance(binder: Binder): Type.GenericInstance {
        val id = makeGenericInstanceId
        makeGenericInstanceId++
        return Type.GenericInstance(binder, id = id)
    }

    fun getTypeArgs(expression: Expression.Call): List<Type>? {
        return null // FIXME
    }

    fun annotationToType(annotation: TypeAnnotation): Type = when(annotation) {
        is TypeAnnotation.Error -> Type.Error
        is TypeAnnotation.Var -> varAnnotationToType(annotation)
        is TypeAnnotation.Ptr -> ptrAnnotationToType(annotation)
        is TypeAnnotation.MutPtr -> mutPtrAnnotationToType(annotation)
        is TypeAnnotation.Application -> typeApplicationAnnotationToType(annotation)
        is TypeAnnotation.Qualified -> qualifiedAnnotationToType(annotation)
        is TypeAnnotation.Function -> TODO()
        is TypeAnnotation.This -> TODO()
        is TypeAnnotation.Union -> TODO()
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
        return resolveTypeVariable(annotation) ?: Type.Error
    }

    fun resolveTypeVariable(annotation: TypeAnnotation.Var): Type? {
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
            is TypeBinding.TypeAlias -> TODO()
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
                if (existing != null) {
                    return lowerType(existing)
                } else {
                    requireUnreachable()
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

enum class Variance {
    INVARIANCE
}
