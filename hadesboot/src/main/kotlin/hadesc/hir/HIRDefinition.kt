package hadesc.hir

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.ptr

sealed class HIRDefinition: HasLocation {
    data class Function(
        override val location: SourceLocation,
        val signature: HIRFunctionSignature,
        val basicBlocks: MutableList<HIRBlock> = mutableListOf(),
    ): HIRDefinition() {
        fun findBlock(label: Name): HIRBlock? {
            return basicBlocks.find { it.name == label }
        }

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

        val fnPtrType get(): Type {
            val functionType = Type.Function(
                from = params.map { it.type },
                to = returnType,
                traitRequirements = null
            ).ptr()
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
    ) : HIRDefinition() {
        val type get() = initializer.type
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
                to = returnType,
                traitRequirements = null
        )
    }

    data class ExternConst(
        override val location: SourceLocation,
        val name: QualifiedName,
        val type: Type,
        val externName: Name,
    ) : HIRDefinition()

    data class Implementation(
            override val location: SourceLocation,
            val traitRequirements: List<TraitRequirement>,
            val typeParams: List<HIRTypeParam>?,
            val typeAliases: Map<Name, Type>,
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
    ) : HIRDefinition() {

        val constructorType get(): Type {
            val instanceConstructorType = Type.Constructor(name)
            val instanceType =
                if (typeParams == null)
                    instanceConstructorType
                else
                    Type.Application(instanceConstructorType, typeParams.map { Type.ParamRef(it.toBinder()) })
            val fnType = Type.Function(
                from = fields.map { it.second },
                to = instanceType,
                traitRequirements = null
            )

            val fnPtrType = Type.Ptr(fnType, isMutable = false)

            return if (typeParams == null)
                fnPtrType
            else
                Type.TypeFunction(
                    params = typeParams.map { Type.Param(it.toBinder()) },
                    body = fnPtrType
                )
        }

        fun constructorRef(location: SourceLocation) = HIRExpression.GlobalRef(
            location,
            constructorType,
            name
        )

        fun instanceType(typeArgs: List<Type> = emptyList()): Type {
            return if (typeParams == null) {
                Type.Constructor(name)
            } else {
                check(typeParams.size == typeArgs.size)
                Type.Application(
                    Type.Constructor(name),
                    typeArgs
                )
            }
        }

        private fun getField(fieldName: Name): Pair<Type, Int> {
            val index = fields.indexOfFirst {
                fieldName == it.first
            }
            val field = fields[index]
            return field.second to index
        }

        fun fieldIndex(fieldName: Name): Int {
            return getField(fieldName).second
        }

        fun fieldType(fieldName: Name): Type {
            return getField(fieldName).first
        }
    }

    fun prettyPrint(): String = when(this) {
        is Function -> {
            "${signature.prettyPrint()} {\n" +
            basicBlocks.joinToString("\n") { block ->
                block.name.text + ":\n  " +
                        block.statements.joinToString("\n  ") { it.prettyPrint() }
            } +
            "\n}"
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
        is Implementation -> "implementation ${traitName.mangle()}[${traitArgs.joinToString(", ") { it.prettyPrint() } }] " +
                "{\n" +
                functions.joinToString("\n") { it.prettyPrint() }.prependIndent("  ") +
                "\n}"
        is ExternConst -> "extern const ${name.mangle()}: ${type.prettyPrint()} = $externName"
    }
}
