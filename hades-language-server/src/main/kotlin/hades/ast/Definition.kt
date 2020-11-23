package hades.ast

import hades.ast.parsing.Token

sealed class Definition : ASTNode {
    data class ImportAs(
        override val meta: ASTMeta,
        val importToken: Token,
        val modulePath: ModulePath,
        val asName: BindingIdentifier,
    ) : Definition()

    data class Def(
        override val meta: ASTMeta,
        val name: BindingIdentifier,
        val typeParams: List<TypeParam>?,
        val params: List<Param>,
        val returnType: TypeAnnotation?,
        val body: Block,
    ) : Definition()

    data class Error(
        override val meta: ASTMeta,
    ) : Definition()
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
