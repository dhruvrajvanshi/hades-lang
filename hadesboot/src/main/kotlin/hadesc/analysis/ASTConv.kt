package hadesc.analysis

import hadesc.ast.Declaration
import hadesc.ast.TypeAnnotation
import hadesc.resolver.Resolver
import hadesc.resolver.TypeBinding
import hadesc.types.Type
import hadesc.types.toSubstitution

class ASTConv(val resolver: Resolver<*>) {

    internal val annotationToTypeCache = MutableNodeMap<TypeAnnotation, Type>()
    fun typeAnnotationToType(annotation: TypeAnnotation): Type = annotation.type()

    private fun TypeAnnotation.type(): Type = annotationToTypeCache.getOrPut(this) {
        when(this) {
            is TypeAnnotation.Application -> typeApplicationToType(this)
            is TypeAnnotation.Array -> Type.Array(itemType.type(), length)
            is TypeAnnotation.Closure -> Type.Closure(from.types(), to.type())
            is TypeAnnotation.FunctionPtr -> Type.FunctionPtr(from.types(), to.type(), traitRequirements = null)
            is TypeAnnotation.MutPtr -> Type.Ptr(to.type(), isMutable = true)
            is TypeAnnotation.Ptr -> Type.Ptr(to.type(), isMutable = false)
            is TypeAnnotation.Error -> Type.Error(location)
            is TypeAnnotation.Qualified -> qualifiedAnnotationToType(this)
            is TypeAnnotation.Select -> selectAnnotationToType(this)
            is TypeAnnotation.Union -> Type.UntaggedUnion(args.types())
            is TypeAnnotation.Var -> varAnnotationToType(this)
        }
    }

    private fun qualifiedAnnotationToType(annotation: TypeAnnotation.Qualified): Type {
        val binding = resolver.resolveQualifiedType(annotation.qualifiedPath)
        if (binding == null) {
            Type.Error(annotation.location)
        }
        return when (binding) {
            is TypeBinding.Enum -> Type.Constructor(resolver.qualifiedName(binding.declaration.name))
            is TypeBinding.Struct -> Type.Constructor(resolver.qualifiedName(binding.declaration.binder))
            is TypeBinding.TypeAlias -> binding.declaration.rhs.type()
            else -> Type.Error(annotation.location)
        }
    }

    private fun selectAnnotationToType(select: TypeAnnotation.Select): Type {
         return when (select.lhs) {
            is TypeAnnotation.Application -> {
                val trait = when (select.lhs.callee) {
                    is TypeAnnotation.Qualified -> {
                        val decl = resolver.resolveDeclaration(select.lhs.callee.qualifiedPath)
                        if (decl !is Declaration.TraitDef) {
                            return Type.Error(select.location)
                        }
                        decl
                    }
                    is TypeAnnotation.Var -> resolver.resolveTraitDef(select.lhs.callee.name)
                    else -> return Type.Error(select.location)
                } ?: return Type.Error(select.location)
                Type.Select(
                    resolver.qualifiedName(trait.name),
                    select.lhs.args.types(),
                    select.rhs.name,
                )
            }
            else -> return Type.Error(select.location)
        }
    }

    private fun varAnnotationToType(annotation: TypeAnnotation.Var): Type {
        val binding = resolver.resolveTypeVariable(annotation.name) ?: return Type.Error(annotation.location)
        return when (binding) {
            is TypeBinding.Enum ->
                Type.Constructor(resolver.qualifiedName(binding.declaration.name))
            is TypeBinding.Struct ->
                Type.Constructor(resolver.qualifiedName(binding.declaration.binder))
            is TypeBinding.TypeAlias -> binding.declaration.rhs.type()
            is TypeBinding.Builtin -> binding.type
            is TypeBinding.AssociatedType -> Type.Error(binding.binder.location)
            is TypeBinding.Trait -> Type.Error(binding.declaration.location)
            is TypeBinding.TypeParam -> Type.Param(binding.binder)
        }
    }

    private fun typeApplicationToType(application: TypeAnnotation.Application): Type {
        val lhsBinding = when (application.callee) {
            is TypeAnnotation.Var -> resolver.resolveTypeVariable(application.callee.name)
            is TypeAnnotation.Qualified -> resolver.resolveQualifiedType(application.callee.qualifiedPath)
           else -> return Type.Error(application.location)
        }
        val (name, _) = when (lhsBinding) {
           is TypeBinding.Struct -> Pair(lhsBinding.declaration.binder, lhsBinding.declaration.typeParams)
           is TypeBinding.Enum -> Pair(lhsBinding.declaration.name, lhsBinding.declaration.typeParams)
            is TypeBinding.TypeAlias -> {
                if (lhsBinding.declaration.typeParams == null) {
                    return Type.Error(application.callee.location)
                } else if (lhsBinding.declaration.typeParams.size != application.args.size) {
                    return Type.Error(application.callee.location)
                } else {
                    val params = lhsBinding.declaration.makeTypeParams()
                    val subst = params.zip(application.args.types()).map { it.first to it.second }.toSubstitution()
                    return lhsBinding.declaration.rhs.type().applySubstitution(subst)
                }
            }
           else -> return Type.Error(application.location)
        }
        val qualifiedName = resolver.qualifiedName(name)
        val constr = Type.Constructor(qualifiedName)

        return Type.Application(
            constr,
            application.args.types()
        )
    }

    private fun List<TypeAnnotation>.types() = map { it.type() }
}