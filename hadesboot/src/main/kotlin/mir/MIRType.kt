package mir

sealed interface MIRType {
    sealed interface Integral {
        sealed interface Signed: Integral
        sealed interface Unsigned: Integral
    }
    object I32: MIRType, Integral.Signed
    object U8: MIRType, Integral.Unsigned

    data class Pointer(val to: MIRType): MIRType

    data class Function(
        val paramTypes: List<MIRType>,
        val returnType: MIRType,
    ): MIRType
}