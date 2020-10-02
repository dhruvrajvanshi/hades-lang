package hadesc.hir

import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class HIRDefinition: HasLocation {
    data class Function(
            override val location: SourceLocation,
            val signature: HIRFunctionSignature,
            val body: HIRBlock
    ): HIRDefinition() {
        val params get() = signature.params
        val returnType get() = signature.returnType
        val name get() = signature.name
        val typeParams get() = signature.typeParams
        val type get(): Type {
            val functionType = Type.Function(
                    from = params.map { it.type },
                    to = returnType,
                    traitRequirements = null
            )
            return if (typeParams != null) {
                Type.TypeFunction(
                        params = typeParams?.map { Type.Param(Binder(Identifier(it.location, it.name))) } ?: emptyList(),
                        body = functionType
                )
            } else {
                functionType
            }
        }
    }

    data class Const(
            override val location: SourceLocation,
            val name: QualifiedName,
            val initializer: HIRExpression
    ) : HIRDefinition()

    data class ExternFunction(
            override val location: SourceLocation,
            val name: QualifiedName,
            val params: List<Type>,
            val returnType: Type,
            val externName: Name
    ) : HIRDefinition() {
        val type get() = Type.Function(
                from = params,
                to = returnType,
                traitRequirements = null
        )
    }

    data class Implementation(
            override val location: SourceLocation,
            val typeParams: List<HIRTypeParam>?,
            val traitName: QualifiedName,
            val traitArgs: List<Type>,
            val functions: List<Function>
    ): HIRDefinition() {
        init {
            require(traitArgs.isNotEmpty())
        }
    }

    data class Struct(
            override val location: SourceLocation,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val fields: List<Pair<Name, Type>>
    ) : HIRDefinition()

    fun prettyPrint(): String = when(this) {
        is Function -> {
            "${signature.prettyPrint()} ${body.prettyPrint()}"
        }
        is ExternFunction -> {
            "extern def ${name.mangle()}(${params.joinToString(", ") {it.prettyPrint()}})" +
                    ": ${returnType.prettyPrint()} = ${externName.text}"
        }
        is Struct -> {
            val typeParamsStr = if (typeParams == null)
                ""
            else "[" + typeParams.joinToString(", ") { it.prettyPrint() } + "]"
            "struct ${name.mangle()}$typeParamsStr {\n" +
                    fields.joinToString("\n") {
                        "  val ${it.first.text}: ${it.second.prettyPrint()}"
                    } +
                    "\n}"
        }
        is Const -> "const ${name.mangle()}: ${initializer.type.prettyPrint()} = ${initializer.prettyPrint()}"
        is Implementation -> "implementation $traitName[${traitArgs.joinToString(", ") { it.prettyPrint() } }] " +
                "{\n" +
                functions.joinToString("\n") { it.prettyPrint() }.prependIndent("  ") +
                "\n}"
    }
}
