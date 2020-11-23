package hades.ast

sealed class TypeAnnotation : ASTNode {
    data class Variable(
        override val meta: ASTMeta,
        val identifier: Identifier
    ) : TypeAnnotation()

    data class RawPtr(
        override val meta: ASTMeta,
        val isMutable: Boolean,
        val toType: TypeAnnotation,
    ) : TypeAnnotation()

    data class TypeApplication(
        override val meta: ASTMeta,
        val callee: TypeAnnotation,
        val args: List<TypeArgument>,
    ) : TypeAnnotation()

    data class Error(override val meta: ASTMeta) : TypeAnnotation()
}

data class TypeArgument(
    override val meta: ASTMeta,
    val label: Identifier?,
    val type: TypeAnnotation,
) : ASTNode