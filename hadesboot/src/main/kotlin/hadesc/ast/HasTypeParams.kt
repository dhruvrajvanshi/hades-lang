package hadesc.ast

sealed interface HasTypeParams {
    val typeParams: List<TypeParam>?
}