package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class HIRDefinition: HasLocation {
    data class Function(
            override val location: SourceLocation,
            val receiverType: Type?,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val params: List<HIRParam>,
            val returnType: Type,
            val body: HIRBlock
    ): HIRDefinition() {
        init {
            require(typeParams == null) { TODO() }
        }
        val type get() = Type.Function(
                receiver = null,
                from = params.map { it.type },
                to = returnType,
                constraints = emptyList(),
                typeParams = null
        )
    }

    data class ExternFunction(
            override val location: SourceLocation,
            val name: QualifiedName,
            val params: List<Type>,
            val returnType: Type,
            val externName: Name
    ) : HIRDefinition() {
        val type get() = Type.Function(
                receiver = null,
                from = params,
                to = returnType,
                constraints = emptyList(),
                typeParams = null
        )
    }

    data class Struct(
            override val location: SourceLocation,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val fields: List<Pair<Name, Type>>
    ) : HIRDefinition()

    fun prettyPrint(): String = when(this) {
        is Function -> TODO()
        is ExternFunction -> TODO()
        is Struct -> TODO()
    }
}
