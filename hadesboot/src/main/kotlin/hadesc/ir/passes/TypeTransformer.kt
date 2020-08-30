package hadesc.ir.passes

import hadesc.analysis.TraitRequirement
import hadesc.types.Type

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
            traitRequirements = type.traitRequirements?.flatMap { lowerTraitRequirement(it) }
    )

    fun lowerTraitRequirement(requirement: TraitRequirement): List<TraitRequirement> {
        return listOf(TraitRequirement(requirement.traitRef, requirement.arguments.map { lowerType(it) }))
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