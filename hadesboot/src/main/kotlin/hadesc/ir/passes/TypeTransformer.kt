package hadesc.ir.passes

import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.types.Type
import hadesc.unit

interface TypeTransformer {
    fun lowerType(type: Type): Type = when (type) {
        is Type.Error -> type
        Type.Void -> lowerVoidType(type)
        Type.Bool -> lowerBoolType(type)
        Type.Size -> lowerSizeType(type)
        Type.Double -> lowerDoubleType(type)
        is Type.Ptr -> lowerRawPtrType(type)
        is Type.Function -> lowerFunctionType(type)
        is Type.Constructor -> lowerTypeConstructor(type)
        is Type.ParamRef -> lowerParamRefType(type)
        is Type.GenericInstance -> lowerGenericInstance(type)
        is Type.Application -> lowerTypeApplication(type)
        is Type.UntaggedUnion -> lowerUntaggedUnionType(type)
        is Type.TypeFunction -> lowerTypeFunction(type)
        is Type.Integral -> lowerIntegralType(type)
        is Type.FloatingPoint -> lowerFloatingPointType(type)
        is Type.Uninferrable -> requireUnreachable()
        is Type.AssociatedTypeRef -> lowerAssociatedTypeRef(type)
        is Type.Select -> lowerSelectType(type)
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

    fun lowerTypeFunction(type: Type.TypeFunction): Type {
        return Type.TypeFunction(
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

    fun lowerFunctionType(type: Type.Function): Type = Type.Function(
            from = type.from.map { lowerType(it) },
            to = lowerType(type.to),
            traitRequirements = type.traitRequirements?.map { lowerTraitRequirement(it) }
    )

    fun lowerTraitRequirement(requirement: TraitRequirement): TraitRequirement {
        return TraitRequirement(requirement.traitRef, requirement.arguments.map { lowerType(it) })
    }

    fun lowerSizeType(type: Type): Type = type

    fun lowerBoolType(type: Type): Type = type

    fun lowerTypeConstructor(type: Type.Constructor): Type = Type.Constructor(
            type.binder,
            name = type.name
    )

    fun lowerParamRefType(type: Type.ParamRef): Type = type

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
        Type.Void -> visitVoidType(type)
        Type.Bool -> visitBoolType(type)
        Type.Size -> visitSizeType(type)
        Type.Double -> visitDoubleType(type)
        is Type.Ptr -> visitRawPtrType(type)
        is Type.Function -> visitFunctionType(type)
        is Type.Constructor -> visitTypeConstructor(type)
        is Type.ParamRef -> visitParamRefType(type)
        is Type.GenericInstance -> visitGenericInstance(type)
        is Type.Application -> visitTypeApplication(type)
        is Type.UntaggedUnion -> visitUntaggedUnionType(type)
        is Type.TypeFunction -> visitTypeFunction(type)
        is Type.Integral -> visitIntegralType(type)
        is Type.FloatingPoint -> visitFloatingPointType(type)
        is Type.Uninferrable -> visitUninferrableType(type)
        is Type.AssociatedTypeRef -> visitAssociatedTypeRef(type)
        is Type.Select -> visitSelectType(type)
    }

    fun visitSelectType(type: Type.Select) = type.traitArgs.forEach {
        visitType(it)
    }

    fun visitAssociatedTypeRef(type: Type.AssociatedTypeRef) = unit

    fun visitUninferrableType(type: Type.Uninferrable) = Unit

    fun visitFloatingPointType(type: Type.FloatingPoint) = Unit

    fun visitIntegralType(type: Type.Integral) = Unit

    fun visitGenericInstance(type: Type.GenericInstance) = Unit

    fun visitTypeFunction(type: Type.TypeFunction) {
        visitType(type.body)
    }

    fun visitDoubleType(type: Type) = Unit

    fun visitUntaggedUnionType(type: Type.UntaggedUnion) {
        type.members.forEach { visitType(it) }
    }

    fun visitFunctionType(type: Type.Function) {
        type.from.forEach { visitType(it) }
        visitType(type.to)
        type.traitRequirements?.forEach { visitTraitRequirement(it) }
    }

    fun visitTraitRequirement(requirement: TraitRequirement) {
        requirement.arguments.forEach { visitType(it) }
    }

    fun visitSizeType(type: Type) = Unit

    fun visitCIntType(type: Type) = Unit

    fun visitBoolType(type: Type) = Unit

    fun visitTypeConstructor(type: Type.Constructor) = Unit

    fun visitParamRefType(type: Type.ParamRef) = Unit

    fun visitTypeApplication(type: Type.Application) {
        visitType(type.callee)
        type.args.map { visitType(it) }
    }

    fun visitVoidType(type: Type) = Unit

    fun visitRawPtrType(type: Type.Ptr) = visitType(type.to)

    fun visitByteType(type: Type) = Unit
}