package hadesc.ir.passes

import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.types.Type
import hadesc.unit

interface TypeTransformer {
    fun lowerType(type: Type): Type = when (type) {
        Type.Error -> type
        Type.Byte -> lowerByteType(type)
        Type.Void -> lowerVoidType(type)
        Type.Bool -> lowerBoolType(type)
        Type.CInt -> lowerCIntType(type)
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

    fun lowerCIntType(type: Type): Type = type

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
        Type.Error -> unit
        Type.Byte -> visitByteType(type)
        Type.Void -> visitVoidType(type)
        Type.Bool -> visitBoolType(type)
        Type.CInt -> visitCIntType(type)
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
    }

    fun visitUninferrableType(type: Type.Uninferrable) {}

    fun visitFloatingPointType(type: Type.FloatingPoint) {
    }

    fun visitIntegralType(type: Type.Integral) {
    }

    fun visitGenericInstance(type: Type.GenericInstance) {
    }

    fun visitTypeFunction(type: Type.TypeFunction) {
        visitType(type.body)
    }

    fun visitDoubleType(type: Type) {
    }

    fun visitUntaggedUnionType(type: Type.UntaggedUnion) {
        type.members.forEach { visitType(it) }
    }

    fun visitFunctionType(type: Type.Function) {
        type.from.forEach() { visitType(it) }
        visitType(type.to)
        type.traitRequirements?.forEach { visitTraitRequirement(it) }
    }

    fun visitTraitRequirement(requirement: TraitRequirement) {
        requirement.arguments.forEach() { visitType(it) }
    }

    fun visitSizeType(type: Type) {}

    fun visitCIntType(type: Type) {}

    fun visitBoolType(type: Type) {}

    fun visitTypeConstructor(type: Type.Constructor) {}

    fun visitParamRefType(type: Type.ParamRef) {}

    fun visitTypeApplication(type: Type.Application) {
        visitType(type.callee)
        type.args.map { visitType(it) }
    }

    fun visitVoidType(type: Type) {}

    fun visitRawPtrType(type: Type.Ptr) = visitType(type.to)

    fun visitByteType(type: Type) {

    }
}