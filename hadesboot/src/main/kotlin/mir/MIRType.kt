package mir

sealed interface MIRType {
    sealed interface Integral {
        sealed interface Signed: Integral
        sealed interface Unsigned: Integral
    }
    object I32: MIRType, Integral.Signed
    object U8: MIRType, Integral.Unsigned

    data class Function(
        val paramTypes: List<MIRType>,
        val returnType: MIRType,
    ): MIRType

    data class Interface(
        /**
         * List of abstract type names required to
         * implement this interface.
         *
         * E.g.
         * interface List {
         *    type Item
         *    ...
         * }
         *
         * will have types = setOf("Item")
         */
        val types: Set<String>,
        val values: Map<String, MIRType>,
    ): MIRType
}