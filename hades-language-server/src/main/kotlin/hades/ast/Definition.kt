package hades.ast

sealed class Definition : ASTNode {
    data class ImportAs(
        override val meta: ASTMeta,
        val modulePath: ModulePath,
        val asName: BindingIdentifier,
    ) : Definition()

    data class Def(
        val name: BindingIdentifier,
        val typeParams: List<TypeParam>?,
        val params: List<Param>,
        val returnType: TypeAnnotation?,
        val body: Block,
    )
}

data class Param(
    override val meta: ASTMeta,
    val name: BindingIdentifier,
    val type: TypeAnnotation?,
) : ASTNode

data class TypeParam(
    override val meta: ASTMeta,
    val name: BindingIdentifier,
) : ASTNode
