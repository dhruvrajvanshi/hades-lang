package hadesc.checker

import hadesc.Name
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.SourceLocation
import hadesc.resolver.TypeBinding
import hadesc.resolver.ValueBinding
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class Checker(val ctx: Context) {
    private val deferredTypeInstantiations = mutableMapOf<Type.Deferred, Type>()

    fun annotationToType(annotation: TypeAnnotation?): Type {
        if (annotation == null) {
            return Type.Error
        }
        return when (annotation.kind) {
            TypeAnnotation.Kind.Error -> Type.Error
            is TypeAnnotation.Kind.Var -> when (identToString(annotation.kind.name)) {
                // TODO: Should be able to override built in types?
                // is it a good idea?
                "Byte" -> Type.Byte
                "Void" -> Type.Void
                "Bool" -> Type.Bool
                else -> when (val typeBinding = ctx.resolver.getTypeBinding(annotation.kind.name)) {
                    is TypeBinding.FunctionDefTypeParam -> Type.ParamRef(
                        typeBinding.binder,
                        typeBinding.paramIndex
                    )
                }
            }
            is TypeAnnotation.Kind.Ptr -> Type.RawPtr(annotationToType(annotation.kind.to))
        }
    }

    fun typeError() = Type.Error

    fun typeOfExpression(expression: Expression): Type = when (expression) {
        is Expression.Error -> typeError()
        is Expression.Var -> typeOfBinding(ctx.resolver.getBinding(expression.name))
        is Expression.Call -> {
            when (val funcType = typeOfExpression(expression.callee)) {
                is Type.Function -> funcType.to
                else -> typeError()
            }
        }
        is Expression.Property -> {
            val lhsType = typeOfExpression(expression.lhs)
            typeOfProperty(lhsType, expression.property)
        }
        is Expression.ByteString -> Type.RawPtr(Type.Byte)
        is Expression.BoolLiteral -> Type.Bool
    }

    fun typeOfProperty(type: Type, propertyName: Identifier): Type = when (type) {
        is Type.Struct -> {
            type.memberTypes[propertyName.name] ?: TODO("${propertyName.location}: No such property")
        }
        is Type.ModuleAlias -> {
            typeOfBinding(ctx.resolver.resolveModuleMember(type.qualifiedName, propertyName))
        }
        else -> {
            TODO("${propertyName.location}: No such property on type $type")
        }
    }

    fun typeOfBinding(binding: ValueBinding): Type = when (binding) {
        is ValueBinding.GlobalFunction -> {
            if (binding.kind.typeParams.isNotEmpty()) {
                Type.GenericFunction(
                    typeParams = binding.kind.typeParams,
                    from = binding.kind.params.map {
                        annotationToType(it.annotation ?: TODO("Type annotation required"))
                    },
                    to = annotationToType(binding.kind.returnType)
                )
            } else {
                Type.Function(
                    from = binding.kind.params.map {
                        annotationToType(it.annotation ?: TODO("Type annotation required"))
                    },
                    to = annotationToType(binding.kind.returnType)
                )
            }
        }
        is ValueBinding.ExternFunction -> Type.Function(
            from = binding.kind.paramTypes.map { annotationToType(it) },
            to = annotationToType(binding.kind.returnType)
        )
        is ValueBinding.FunctionParam -> annotationToType(binding.param.annotation ?: TODO("Annotation required"))
        is ValueBinding.ImportAs -> {
            Type.ModuleAlias(binding.aliasedModule)
        }
        is ValueBinding.ValBinding -> typeOfExpression(binding.kind.rhs)
        is ValueBinding.Struct -> typeOfStructConstructor(binding.kind)
    }

    private fun identToString(identifier: Identifier): String {
        return identifier.name.text
    }

    fun typeOfStructInstance(kind: Declaration.Kind.Struct): Type {
        val name = ctx.resolver.getBinding(kind.binder.identifier).qualifiedName
        val memberTypes = mutableMapOf<Name, Type>()
        for (member in kind.members) {
            val exhaustive = when (member) {
                Declaration.Kind.Struct.Member.Error -> {
                }
                is Declaration.Kind.Struct.Member.Field -> {
                    val type = annotationToType(member.typeAnnotation)
                    memberTypes[member.binder.identifier.name] = type
                    Unit
                }
            }
        }
        return Type.Struct(
            name,
            memberTypes
        )
    }

    fun typeOfStructConstructor(kind: Declaration.Kind.Struct): Type {

        val memberTypeList = buildList {
            kind.members.forEach {
                if (it is Declaration.Kind.Struct.Member.Field) {
                    add(annotationToType(it.typeAnnotation))
                }
            }
        }
        return Type.Function(from = memberTypeList, to = typeOfStructInstance(kind))
    }

    fun checkDeclaration(declaration: Declaration) = when (declaration.kind) {
        Declaration.Kind.Error -> {
        }
        is Declaration.Kind.ImportAs -> checkImportAsDeclaration(declaration, declaration.kind)
        is Declaration.Kind.FunctionDef -> checkFunctionDef(declaration, declaration.kind)
        is Declaration.Kind.ExternFunctionDef -> checkExternFunctionDef(declaration, declaration.kind)
        is Declaration.Kind.Struct -> checkStructDeclaration(declaration, declaration.kind)
    }

    private fun checkStructDeclaration(declaration: Declaration, kind: Declaration.Kind.Struct) {
        // TODO
    }

    private fun checkExternFunctionDef(declaration: Declaration, kind: Declaration.Kind.ExternFunctionDef) {
        // TODO
    }

    private fun checkFunctionDef(declaration: Declaration, kind: Declaration.Kind.FunctionDef) {
        for (param in kind.params) {
            if (param.annotation != null) {
                annotationToType(param.annotation)
            }
        }
        // TODO
    }

    private fun checkImportAsDeclaration(declaration: Declaration, kind: Declaration.Kind.ImportAs) {
        // TODO
    }

    fun isGenericCallSite(call: Expression.Call): Boolean {
        val ty = typeOfExpression(call.callee)
        return ty is Type.GenericFunction
    }

    fun getGenericSpecializedFunctionType(
        function: Declaration.Kind.FunctionDef,
        callLocation: SourceLocation,
        args: List<Arg>
    ): Type.Function {
        val subst = mutableMapOf<Type.ParamRef, Type>()
        val paramTypeInstantiations = function.params.map {
            instantiateType(annotationToType(it.annotation), callLocation)
        }
        val instantiatedReturnType = instantiateType(annotationToType(function.returnType), callLocation)

        assert(paramTypeInstantiations.size == args.size) { "${callLocation}: Wrong number of args" }
        for ((arg, paramType) in args.zip(paramTypeInstantiations)) {
            equateTypes(typeOfExpression(arg.expression), paramType)
        }

        return Type.Function(
            from = paramTypeInstantiations.map { substituteSpecializations(it) },
            to = substituteSpecializations(instantiatedReturnType)
        )
    }

    private fun substituteSpecializations(instantiatedType: Type): Type = when (instantiatedType) {
        Type.Error,
        Type.Byte,
        Type.Void,
        Type.Bool,
        is Type.RawPtr -> Type.RawPtr(substituteSpecializations((instantiatedType as Type.RawPtr).to))
        is Type.Function -> Type.Function(
            from = instantiatedType.from.map { substituteSpecializations(it) },
            to = substituteSpecializations(instantiatedType.to)
        )
        is Type.GenericFunction -> TODO()
        is Type.Struct -> TODO()
        is Type.ModuleAlias -> TODO()
        is Type.ParamRef -> TODO("Cannot generate fully specialized type")
        is Type.Deferred -> deferredTypeInstantiations[instantiatedType] ?: TODO("Couldn't infer generic instantiation")
    }

    private fun equateTypes(t1: Type, t2: Type) {
        when {
            t2 is Type.Deferred && t1 is Type.Deferred ->
                TODO("Cannot infer generic type")
            t2 is Type.Deferred -> {
                deferredTypeInstantiations[t2] = t1
            }
            else -> {
            }
        }
    }

    private fun instantiateType(type: Type, callLocation: SourceLocation): Type = when (type) {
        Type.Error,
        Type.Byte,
        Type.Void,
        Type.Bool -> type
        is Type.RawPtr -> Type.RawPtr(instantiateType(type.to, callLocation))
        is Type.Function -> Type.Function(
            from = type.from.map { instantiateType(it, callLocation) },
            to = instantiateType(type.to, callLocation)
        )
        is Type.GenericFunction ->
            TODO("Higher order genrerics; Need to avoid substituting param refs bound by this function")
        is Type.Struct -> TODO()
        is Type.ModuleAlias -> TODO()
        is Type.ParamRef -> newDeferredType(type, callLocation)
        is Type.Deferred -> type
    }

    private fun newDeferredType(paramRef: Type.ParamRef, callLocation: SourceLocation): Type {
        return Type.Deferred(paramRef, callLocation)
    }

}