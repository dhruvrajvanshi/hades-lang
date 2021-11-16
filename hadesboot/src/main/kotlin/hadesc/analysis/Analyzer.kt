package hadesc.analysis

import hadesc.Name
import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.frontend.PropertyBinding
import hadesc.hir.BinaryOperator
import hadesc.resolver.TypeBinding
import hadesc.types.Type
import hadesc.types.toSubstitution

class Analyzer(
        private val ctx: Context
) {
    private val annotationToTypeCache = MutableNodeMap<TypeAnnotation, Type>()
    private fun TypeAnnotation.toType() = annotationToType(this)

    fun annotationToType(annotation: TypeAnnotation) = annotationToTypeCache.getOrPut(annotation) {
        annotationToTypeWorker(annotation)
    }
    private fun annotationToTypeWorker(annotation: TypeAnnotation): Type = when (annotation) {
        is TypeAnnotation.Application -> {
            val callee = annotation.callee.toType()
            if (callee is Type.TypeFunction) {
                if (annotation.args.size > callee.params.size) {
                    ctx.report(annotation.callee, Diagnostic.Kind.TooManyTypeArgs)
                }
                val args = annotation.args.map { it.toType() }
                val substitution = callee.params.zip(args).toSubstitution()
                callee.body.applySubstitution(substitution)
            } else {
                ctx.report(annotation.callee, Diagnostic.Kind.TooManyTypeArgs)
                Type.Error(annotation.location)
            }
        }
        is TypeAnnotation.Error -> TODO()
        is TypeAnnotation.Function -> Type.Function(
            annotation.from.map { it.toType() },
            traitRequirements = null,
            annotationToType(annotation.to),
        )
        is TypeAnnotation.MutPtr -> Type.Ptr(annotationToType(annotation.to), isMutable = true)
        is TypeAnnotation.Ptr -> Type.Ptr(annotationToType(annotation.to), isMutable = false)
        is TypeAnnotation.Qualified -> {
            val modulePath = annotation.qualifiedPath.dropLast(1)
            val typeName = annotation.qualifiedPath.identifiers.last()
            if (modulePath.size == 1) {
                resolveTypeMemberOfModuleAlias(annotation, modulePath[0], typeName)
            } else {
                resolveTypeMemberOfQualifiedModule(annotation, modulePath, typeName)
            }
        }
        is TypeAnnotation.Select -> TODO()
        is TypeAnnotation.Union -> TODO()
        is TypeAnnotation.Var -> {
            when (val binding = ctx.resolver.resolveTypeVariable(annotation.name)) {
                is TypeBinding.AssociatedType -> TODO()
                is TypeBinding.Builtin -> binding.type
                is TypeBinding.SealedType -> TODO()
                is TypeBinding.Struct -> {
                    val body = Type.Constructor(ctx.resolver.qualifiedStructName(binding.declaration))
                    if (binding.declaration.typeParams == null) {
                        body
                    } else {
                        Type.TypeFunction(
                            binding.declaration.typeParams.map { Type.Param(it.binder) },
                            body
                        )
                    }
                }
                is TypeBinding.Trait -> TODO()
                is TypeBinding.TypeAlias -> {
                    val aliasDecl = binding.declaration
                    val bodyType = annotationToType(aliasDecl.rhs)
                    if (aliasDecl.typeParams != null) {
                        Type.TypeFunction(
                            aliasDecl.typeParams.map { Type.Param(it.binder) },
                            bodyType
                        )
                    } else {
                        bodyType
                    }
                }
                is TypeBinding.TypeParam -> Type.ParamRef(binding.binder)
                null -> {
                    ctx.diagnosticReporter.report(annotation.location, Diagnostic.Kind.UnboundType(annotation.name.name))
                    Type.Error(annotation.location)
                }
            }
        }
    }

    private fun resolveTypeMemberOfModuleAlias(
        annotation: TypeAnnotation.Qualified,
        moduleName: Identifier,
        typeName: Identifier
    ): Type {
        val sourceFile = ctx.resolver.resolveModuleAlias(moduleName)
        if (sourceFile == null) {
            ctx.report(moduleName, Diagnostic.Kind.NoSuchModule)
            return Type.Error(moduleName.location)
        }
        return when (val typeBinding = ctx.resolver.findTypeInSourceFile(typeName, sourceFile)) {
            null -> {
                ctx.report(annotation, Diagnostic.Kind.NoSuchMember)
                Type.Error(annotation.location)
            }
            else -> typeOfTypeBinding(typeBinding)
        }
    }

    private fun resolveTypeMemberOfQualifiedModule(annotation: TypeAnnotation, modulePath: QualifiedPath, typeName: Identifier): Type {
        val sourceFile = ctx.resolveSourceFile(modulePath)
        if (sourceFile == null) {
            ctx.report(modulePath, Diagnostic.Kind.NoSuchModule)
            return Type.Error(annotation.location)
        }
        return when (val typeBinding = ctx.resolver.findTypeInSourceFile(typeName, sourceFile)) {
            null -> {
                ctx.report(annotation, Diagnostic.Kind.NoSuchMember)
                Type.Error(annotation.location)
            }
            else -> typeOfTypeBinding(typeBinding)
        }
    }

    private fun typeOfTypeBinding(typeBinding: TypeBinding): Type = when(typeBinding) {
        is TypeBinding.AssociatedType -> TODO()
        is TypeBinding.Builtin -> TODO()
        is TypeBinding.SealedType -> TODO()
        is TypeBinding.Struct -> Type.Constructor(ctx.resolver.qualifiedStructName(typeBinding.declaration))
        is TypeBinding.Trait -> TODO()
        is TypeBinding.TypeAlias -> {
            val aliasDecl = typeBinding.declaration
            val body = aliasDecl.rhs.toType()

            if (aliasDecl.typeParams != null) {
                Type.TypeFunction(
                    aliasDecl.typeParams.map { Type.Param(it.binder) },
                    body
                )
            } else {
                body
            }
        }
        is TypeBinding.TypeParam -> TODO()
    }

    private val typeArgsCache = MutableNodeMap<Expression, List<Type>>()
    fun getTypeArgs(expression: Expression): List<Type>? {
        return typeArgsCache[expression]
    }

    fun setTypeArgs(expression: Expression, args: List<Type>) {
        check(typeArgsCache[expression] == null)
        typeArgsCache[expression] = args
    }

    fun getSealedTypeConstructorBinding(expression: Expression): PropertyBinding.SealedTypeCaseConstructor? = null
    fun getClosureCaptures(expression: Expression.Closure): ClosureCaptures {
        TODO()
    }

    fun getSealedTypePayloadType(declaration: Declaration.SealedType): Type {
        TODO()
    }

    fun getParamType(param: Param): Type {
        TODO()
    }

    fun getSealedTypeDeclaration(discriminantType: Type): Declaration.SealedType {
        TODO()
    }

    private val resolvePropertyBindingCache = MutableNodeMap<Expression.Property, PropertyBinding>()
    fun resolvePropertyBinding(expr: Expression.Property): PropertyBinding {
        return checkNotNull(resolvePropertyBindingCache[expr])
    }

    private val typeOfBinderCache = MutableNodeMap<Binder, Type>()
    fun typeOfBinder(binder: Binder): Type? {
        return typeOfBinderCache[binder]
    }

    fun getCallReceiver(expression: Expression.Call): Expression? {
        return null
    }

    private val typeOfExpressionCache = MutableNodeMap<Expression, Type>()
    fun typeOfExpression(expression: Expression): Type {
        return checkNotNull(typeOfExpressionCache[expression]) {
            "Type not inferred for expression at location ${expression.location}"
        }
    }

    fun assignExpressionTypes(expressionTypes: List<Pair<Expression, Type>>) {
        for ((expression, type) in expressionTypes) {
            check(typeOfExpressionCache[expression] == null)
            typeOfExpressionCache[expression] = type
        }
    }

    fun assignBinderTypes(binderTypes: List<Pair<Binder, Type>>) {
        for ((expression, type) in binderTypes) {
            check(typeOfBinderCache[expression] == null)
            typeOfBinderCache[expression] = type
        }
    }

    fun assignPropertyBindings(propertyBindings: List<Pair<Expression.Property, PropertyBinding>>) {
        for ((expression, binding) in propertyBindings) {
            check(resolvePropertyBindingCache[expression] == null)
            resolvePropertyBindingCache[expression] = binding
        }
    }
}

data class ClosureCaptures(
    val values: Map<Binder, Type>,
    val types: Set<Binder>
)

typealias op = BinaryOperator

val BIN_OP_RULES: Map<Pair<op, Type>, Pair<Type, Type>> = mapOf(

        (op.AND to Type.Bool) to (Type.Bool to Type.Bool),
        (op.OR to Type.Bool) to (Type.Bool to Type.Bool),


        (op.EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
        (op.NOT_EQUALS to Type.Bool) to (Type.Bool to Type.Bool),
)

data class Discriminant(
    val index: Int,
    val name: Name,
    val params: List<Pair<Name, Type>>
)
