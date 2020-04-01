package hadesc.types

import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.TypeParam
import hadesc.qualifiedname.QualifiedName

sealed class Type {
    object Error : Type()
    object Byte : Type()
    object Void : Type()
    object Bool : Type()
    data class RawPtr(val to: Type) : Type()
    data class Function(val from: List<Type>, val to: Type) : Type()
    data class GenericFunction(
        val typeParams: List<TypeParam>,
        val from: List<Type>,
        val to: Type
    ) : Type()

    data class Struct(val name: QualifiedName, val memberTypes: Map<Name, Type>) : Type()

    // this isn't a real runtime type
    // any property accesses on this should
    // be resolved to a fully qualified global
    // name
    data class ModuleAlias(
        val qualifiedName: QualifiedName
    ) : Type()

    data class ParamRef(
        val binder: Binder,
        val typeParamIndex: Int
    ) : Type()
}
