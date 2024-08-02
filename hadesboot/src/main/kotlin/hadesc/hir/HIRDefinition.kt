package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Substitution
import hadesc.types.Type
import hadesc.types.ptr

sealed class HIRDefinition : HasLocation {
    data class Function(
        override val location: SourceLocation,
        val signature: HIRFunctionSignature,
        val basicBlocks: MutableList<HIRBlock> = mutableListOf()
    ) : HIRDefinition() {
        private fun findBlock(label: Name): HIRBlock? {
            return basicBlocks.find { it.name == label }
        }

        fun findBlockOrThrow(label: Name): HIRBlock {
            return checkNotNull(findBlock(label)) {
                "Block with label: ${label.text} was not found in this function"
            }
        }

        val params get() = signature.params
        val returnType get() = signature.returnType
        val name get() = signature.name
        val typeParams get() = signature.typeParams
        val type get(): Type {
            val functionPtrType = Type.FunctionPtr(
                from = params.map { it.type },
                to = returnType,
            )
            return if (typeParams != null) {
                Type.ForAll(
                    params = typeParams?.map { Type.Param(it.toBinder()) } ?: emptyList(),
                    body = functionPtrType
                )
            } else {
                functionPtrType
            }
        }

        val fnPtrType get(): Type {
            val functionPtrType = Type.FunctionPtr(
                from = params.map { it.type },
                to = returnType,
            ).ptr()
            return if (typeParams != null) {
                Type.ForAll(
                    params = typeParams?.map { Type.Param(it.toBinder()) } ?: emptyList(),
                    body = functionPtrType
                )
            } else {
                functionPtrType
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
        val type get() = Type.FunctionPtr(
            from = params,
            to = returnType,
        )
    }

    data class ExternConst(
        override val location: SourceLocation,
        val name: QualifiedName,
        val type: Type,
        val externName: Name
    ) : HIRDefinition()

    data class Struct(
        override val location: SourceLocation,
        val name: QualifiedName,
        val typeParams: List<HIRTypeParam>?,
        val fields: List<Pair<Name, Type>>
    ) : HIRDefinition() {

        val constructorType get(): Type {
            val instanceConstructorType = Type.Constructor(name)
            val instanceType =
                if (typeParams == null) {
                    instanceConstructorType
                } else {
                    Type.Application(instanceConstructorType, typeParams.map { Type.Param(it.toBinder()) })
                }
            val fnType = Type.FunctionPtr(
                from = fields.map { it.second },
                to = instanceType,
            )

            return if (typeParams == null) {
                fnType
            } else {
                Type.ForAll(
                    params = typeParams.map { Type.Param(it.toBinder()) },
                    body = fnType
                )
            }
        }

        fun instanceType(typeArgs: List<Type>? = null): Type {
            return if (typeParams == null) {
                require(typeArgs == null)
                Type.Constructor(name)
            } else {
                requireNotNull(typeArgs)
                check(typeParams.size == typeArgs.size)
                Type.Application(
                    Type.Constructor(name),
                    typeArgs
                )
            }
        }

        fun getField(fieldName: Name, typeArgs: List<Type>? = null): Pair<Type, Int> {
            val index = fields.indexOfFirst {
                fieldName == it.first
            }
            val field = fields[index]
            val ty =
                if (typeArgs != null) {
                    field.second.applySubstitution(
                        Substitution.of(
                            typeParams,
                            typeArgs
                        )
                    )
                } else {
                    field.second
                }
            return ty to index
        }

        fun fieldIndex(fieldName: Name): Int {
            return getField(fieldName).second
        }

        fun fieldType(fieldName: Name, typeArgs: List<Type>? = null): Type {
            return getField(fieldName, typeArgs).first
        }
    }

    fun prettyPrint(): String = when (this) {
        is Function -> {
            "${signature.prettyPrint()} {\n" +
                basicBlocks.joinToString("\n") { block ->
                    block.name.text + ":\n  " +
                        block.statements.joinToString("\n  ") { it.prettyPrint() }
                } +
                "\n}"
        }
        is ExternFunction -> {
            "extern fn ${name.mangle()}(${params.joinToString(", ") {it.prettyPrint()}})" +
                ": ${returnType.prettyPrint()} = ${externName.text}"
        }
        is Struct -> {
            val typeParamsStr = if (typeParams == null) {
                ""
            } else {
                "[" + typeParams.joinToString(", ") { it.prettyPrint() } + "]"
            }
            "struct ${name.mangle()}$typeParamsStr {\n" +
                fields.joinToString("\n") {
                    "  ${it.first.text}: ${it.second.prettyPrint()}"
                } +
                "\n}"
        }
        is Const -> "const ${name.mangle()}: ${initializer.type.prettyPrint()} = ${initializer.prettyPrint()}"
        is ExternConst -> "extern const ${name.mangle()}: ${type.prettyPrint()} = $externName"
    }
}
