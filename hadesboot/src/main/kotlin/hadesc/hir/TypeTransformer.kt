package hadesc.hir

import hadesc.analysis.TraitRequirement
import hadesc.types.Type
import hadesc.unit

interface TypeTransformer {
    fun lowerType(type: Type): Type = when (type) {
        is Type.Error -> type
        Type.Void -> lowerVoidType(type)
        Type.Bool -> lowerBoolType(type)
        is Type.Size -> lowerSizeType(type)
        is Type.Ptr -> lowerRawPtrType(type)
        is Type.FunctionPtr -> lowerFunctionType(type)
        is Type.Constructor -> lowerTypeConstructor(type)
        is Type.Param -> lowerParamRefType(type)
        is Type.GenericInstance -> lowerGenericInstance(type)
        is Type.Application -> lowerTypeApplication(type)
        is Type.UntaggedUnion -> lowerUntaggedUnionType(type)
        is Type.ForAll -> lowerTypeFunction(type)
        is Type.Integral -> lowerIntegralType(type)
        is Type.FloatingPoint -> lowerFloatingPointType(type)
        is Type.AssociatedTypeRef -> lowerAssociatedTypeRef(type)
        is Type.Select -> lowerSelectType(type)
        is Type.Closure -> lowerClosureType(type)
        is Type.Ref -> lowerRefType(type)
        is Type.Array -> lowerArrayType(type)
        Type.CChar -> lowerCCharType(type)
    }

    fun lowerCCharType(type: Type): Type = type

    fun lowerArrayType(type: Type.Array): Type = Type.Array(
        lowerType(type.itemType),
        type.length
    )

    fun lowerRefType(type: Type.Ref): Type = Type.Ref(lowerType(type.inner))

    fun lowerClosureType(type: Type.Closure): Type {
        return Type.Closure(
            from = type.from.map { lowerType(it) },
            to = lowerType(type.to)
        )
    }

    fun lowerSelectType(type: Type.Select): Type {
        return type.copy(
            traitArgs = type.traitArgs.map { lowerType(it) }
        )
    }

    fun lowerAssociatedTypeRef(type: Type.AssociatedTypeRef): Type {
        return type
    }

    fun lowerFloatingPointType(type: Type.FloatingPoint): Type {
        return type
    }

    fun lowerIntegralType(type: Type.Integral): Type {
        return type
    }

    fun lowerGenericInstance(type: Type.GenericInstance): Type {
        return type
    }

    fun lowerTypeFunction(type: Type.ForAll): Type {
        return Type.ForAll(
            params = type.params,
            body = lowerType(type.body)
        )
    }

    fun lowerDoubleType(type: Type): Type {
        return type
    }

    fun lowerUntaggedUnionType(type: Type.UntaggedUnion): Type {
        return Type.UntaggedUnion(type.members.map { lowerType(it) })
    }

    fun lowerFunctionType(type: Type.FunctionPtr): Type = Type.FunctionPtr(
        from = type.from.map { lowerType(it) },
        to = lowerType(type.to),
        traitRequirements = type.traitRequirements?.map { lowerTraitRequirement(it) }
    )

    fun lowerTraitRequirement(requirement: TraitRequirement): TraitRequirement {
        return TraitRequirement(requirement.traitRef, requirement.arguments.map { lowerType(it) }, negated = requirement.negated)
    }

    fun lowerSizeType(type: Type): Type = type

    fun lowerBoolType(type: Type): Type = type

    fun lowerTypeConstructor(type: Type.Constructor): Type = type

    fun lowerParamRefType(type: Type.Param): Type = type

    fun lowerTypeApplication(type: Type.Application): Type = Type.Application(
        callee = lowerType(type.callee) as Type.Constructor,
        args = type.args.map { lowerType(it) }
    )

    fun lowerVoidType(type: Type): Type = type

    fun lowerRawPtrType(type: Type.Ptr): Type = Type.Ptr(lowerType(type.to), isMutable = type.isMutable)

    fun lowerByteType(type: Type): Type = type
}

interface TypeVisitor {
    fun visitType(type: Type): Unit = when (type) {
        is Type.Error -> unit
        is Type.Void -> visitVoidType(type)
        is Type.Bool -> visitBoolType(type)
        is Type.Size -> visitSizeType(type)
        is Type.Ptr -> visitRawPtrType(type)
        is Type.FunctionPtr -> visitFunctionType(type)
        is Type.Constructor -> visitTypeConstructor(type)
        is Type.Param -> visitParamRefType(type)
        is Type.GenericInstance -> visitGenericInstance(type)
        is Type.Application -> visitTypeApplication(type)
        is Type.UntaggedUnion -> visitUntaggedUnionType(type)
        is Type.ForAll -> visitTypeFunction(type)
        is Type.Integral -> visitIntegralType(type)
        is Type.FloatingPoint -> visitFloatingPointType(type)
        is Type.AssociatedTypeRef -> visitAssociatedTypeRef(type)
        is Type.Select -> visitSelectType(type)
        is Type.Closure -> visitClosureType(type)
        is Type.Ref -> visitRefType(type)
        is Type.Array -> visitArrayType(type)
        Type.CChar -> visitCCharType(type)
    }

    fun visitCCharType(type: Type) = Unit

    fun visitArrayType(type: Type.Array) {
        visitType(type.itemType)
    }

    fun visitRefType(type: Type.Ref) {
        visitType(type.inner)
    }

    fun visitClosureType(type: Type.Closure) {
        type.from.forEach { visitType(it) }
        visitType(type.to)
    }

    fun visitSelectType(type: Type.Select) = type.traitArgs.forEach {
        visitType(it)
    }

    fun visitAssociatedTypeRef(type: Type.AssociatedTypeRef) = unit

    fun visitFloatingPointType(type: Type.FloatingPoint) = Unit

    fun visitIntegralType(type: Type.Integral) = Unit

    fun visitGenericInstance(type: Type.GenericInstance) = Unit

    fun visitTypeFunction(type: Type.ForAll) {
        visitType(type.body)
    }

    fun visitUntaggedUnionType(type: Type.UntaggedUnion) {
        type.members.forEach { visitType(it) }
    }

    fun visitFunctionType(type: Type.FunctionPtr) {
        type.from.forEach { visitType(it) }
        visitType(type.to)
        type.traitRequirements?.forEach { visitTraitRequirement(it) }
    }

    fun visitTraitRequirement(requirement: TraitRequirement) {
        requirement.arguments.forEach { visitType(it) }
    }

    fun visitSizeType(type: Type.Size) = Unit

    fun visitBoolType(type: Type.Bool) = Unit

    fun visitTypeConstructor(type: Type.Constructor) = Unit

    fun visitParamRefType(type: Type.Param) = Unit

    fun visitTypeApplication(type: Type.Application) {
        visitType(type.callee)
        type.args.map { visitType(it) }
    }

    fun visitVoidType(type: Type) = Unit

    fun visitRawPtrType(type: Type.Ptr) = visitType(type.to)
}
