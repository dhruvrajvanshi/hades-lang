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
            val constraintParams: List<HIRConstraintParam>?,
            val params: List<HIRParam>,
            val returnType: Type,
            val body: HIRBlock
    ): HIRDefinition() {
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

    data class Implementation(
            override val location: SourceLocation,
            val name: QualifiedName,
            val typeParams: List<HIRTypeParam>?,
            val interfaceRef: HIRInterfaceRef,
            val forType: Type,
            val members: List<Function>,
            val constraintParams: List<HIRConstraintParam>?
    ) : HIRDefinition()

    fun prettyPrint(): String = when(this) {
        is Function -> {
            val whereStr = if (constraintParams == null) {
                ""
            } else {
                "where (${constraintParams.joinToString(", ") {it.prettyPrint()} })"
            }
            val typeParamsStr = if (typeParams == null)
                ""
            else "[${typeParams.joinToString(", ") { it.prettyPrint() }}] $whereStr "
            val thisParamStr = if (receiverType != null) {
                "this: ${receiverType.prettyPrint()}" + if (params.isEmpty()) "" else ", "
            } else ""
            "def ${name.mangle()}$typeParamsStr($thisParamStr${params.joinToString(", ") {it.prettyPrint()}})" +
                    ": ${returnType.prettyPrint()} ${body.prettyPrint()}"
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
    }
}
