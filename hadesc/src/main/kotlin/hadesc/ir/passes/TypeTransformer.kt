package hadesc.ir.passes

import hadesc.assertions.requireUnreachable
import hadesc.types.Type

interface TypeTransformer {
    fun lowerType(type: Type): Type = when (type) {
        Type.Error -> requireUnreachable() {
            TODO()
        }
        Type.Byte -> lowerByteType(type)
        Type.Void -> lowerVoidType(type)
        Type.Bool -> lowerBoolType(type)
        Type.CInt -> lowerCIntType(type)
        Type.Size -> lowerSizeType(type)
        is Type.RawPtr -> lowerRawPtrType(type)
        is Type.Function -> lowerFunctionType(type)
        is Type.Constructor -> lowerTypeConstructor(type)
        is Type.ParamRef -> lowerParamRefType(type)
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> lowerTypeApplication(type)
        is Type.ThisRef -> lowerThisRefType(type)
        is Type.UntaggedUnion -> lowerUntaggedUnionType(type)
    }

    fun lowerUntaggedUnionType(type: Type.UntaggedUnion): Type {
        return Type.UntaggedUnion(type.members.map { lowerType(it) })
    }

    fun lowerFunctionType(type: Type.Function): Type = Type.Function(
            receiver = type.receiver?.let { lowerType(it) },
            typeParams = type.typeParams,
            from = type.from.map { lowerType(it) },
            to = lowerType(type.to)
    )

    fun lowerSizeType(type: Type): Type = type

    fun lowerCIntType(type: Type): Type = type

    fun lowerBoolType(type: Type): Type = type

    fun lowerTypeConstructor(type: Type.Constructor): Type = Type.Constructor(
            type.binder,
            name = type.name,
            params = type.params
    )

    fun lowerParamRefType(type: Type.ParamRef): Type = type

    fun lowerTypeApplication(type: Type.Application): Type = Type.Application(
            callee = lowerType(type.callee) as Type.Constructor,
            args = type.args.map { lowerType(it) }
    )

    fun lowerThisRefType(type: Type.ThisRef): Type = type

    fun lowerVoidType(type: Type): Type = type

    fun lowerRawPtrType(type: Type.RawPtr): Type = Type.RawPtr(lowerType(type.to))

    fun lowerByteType(type: Type): Type = type
}