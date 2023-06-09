package mir

sealed interface MIRType {
    object I32: MIRType
    data class Function(
        val paramTypes: List<MIRType>,
        val returnType: MIRType,
    ): MIRType
    data class Struct(
        val values: Map<String, MIRType>,
    )
}