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
        val constraintParams get() = signature.constraintParams
        val name get() = signature.name
        val typeParams get() = signature.typeParams
        val type get(): Type {
            val functionType = Type.Function(
                    from = params.map { it.type },
                    to = returnType

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

    data class ExternFunction(
            override val location: SourceLocation,
            val name: QualifiedName,
            val params: List<Type>,
            val returnType: Type,
            val externName: Name
    ) : HIRDefinition() {
        val type get() = Type.Function(
                from = params,
                to = returnType
        )
    }

    data class Struct(
            override val location: SourceLocation,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val fields: List<Pair<Name, Type>>
    ) : HIRDefinition()

    data class Implementation(
            override val location: SourceLocation,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val interfaceRef: HIRInterfaceRef,
            val forType: Type,
            val members: List<Function>,
            val constraintParams: List<HIRConstraintParam>?
    ) : HIRDefinition() {
        init {
            require(constraintParams == null || constraintParams.isNotEmpty())
        }
    }

    data class Interface(
            override val location: SourceLocation,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val constraintParams: List<HIRConstraintParam>?,
            val signatures: List<HIRFunctionSignature>
    ) : HIRDefinition() {
        init {
            require(constraintParams == null || constraintParams.isNotEmpty())
        }
    }

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
        is Implementation -> {
            val whereStr = if (constraintParams == null) {
                ""
            } else {
                "where (${constraintParams.joinToString(", ") {it.prettyPrint()} })"
            }
            val typeParamsStr = if (typeParams == null)
                ""
            else "[${typeParams.joinToString(", ") { it.prettyPrint() }}] $whereStr -> "
            "implementation ${name.mangle()}: $typeParamsStr${interfaceRef.prettyPrint()} for ${forType.prettyPrint()} {\n${
                members.joinToString("\n") {
                    it.prettyPrint().prependIndent("  ")
                }
            }\n}"
        }
        is Interface -> {
            val whereStr = if (constraintParams == null) {
                ""
            } else {
                "where (${constraintParams.joinToString(", ") {it.prettyPrint()} })"
            }
            val typeParamsStr = if (typeParams == null)
                ""
            else "[${typeParams.joinToString(", ") { it.prettyPrint() }}] $whereStr "
            val signaturesStr = signatures.joinToString("\n") { it.prettyPrint() }.prependIndent("  ")
            "interface ${name.mangle()}$typeParamsStr {\n$signaturesStr\n}"
        }
    }
}
