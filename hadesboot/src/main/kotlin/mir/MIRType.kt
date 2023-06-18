package mir

sealed interface MIRType {
    object I32: MIRType
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