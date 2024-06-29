package hadesc.codegen.c

import hadesboot.prettyprint.prettyPrint
import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.hir.HIRBlock
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIRModule
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

class HIRToC(
    private val hirModule: HIRModule
) {
    private val declarations = mutableListOf<CNode>(
        CNode.Include("stddef.h"),
        CNode.Include("stdbool.h"),
        CNode.Include("stdint.h"),
    )

    fun lower(): String {
        for (definition in hirModule.definitions.sortedBy { it.interfaceSortOrder() }) {
            lowerDefinitionInterface(definition)
        }
        for (definition in hirModule.definitions) {
            lowerDefinitionImplementation(definition)
        }
        return declarationsToPPNode(declarations).prettyPrint()
    }

    private fun lowerDefinitionInterface(definition: HIRDefinition): Unit = when (definition) {
        is HIRDefinition.Const -> {}
        is HIRDefinition.ExternConst -> lowerExternConstInterface(definition)
        is HIRDefinition.ExternFunction -> lowerExternFunctionInterface(definition)
        is HIRDefinition.Function -> lowerFunctionInterface(definition)
        is HIRDefinition.Implementation -> requireUnreachable { "Expected previous phases to eliminate interfaces" }
        is HIRDefinition.Struct -> lowerStructInterface(definition)
    }

    private fun lowerExternConstInterface(definition: HIRDefinition.ExternConst) {
        val type = lowerType(definition.type)
        declarations.add(CNode.ExternConst(definition.name.c(), type))
    }

    private fun lowerExternFunctionInterface(definition: HIRDefinition.ExternFunction) {
        val returnType = lowerType(definition.returnType)
        val parameters = definition.params.map { lowerType(it) }
        declarations.add(
            CNode.FnSignature(
                name = definition.name.c(),
                returnType = returnType,
                parameters = parameters,
                isExtern = true
            )
        )
    }

    private fun lowerFunctionInterface(definition: HIRDefinition.Function) {
        val returnType = lowerType(definition.returnType)
        val parameters = definition.params.map { lowerType(it.type) }
        declarations.add(
            CNode.FnSignature(
                name = definition.name.c(),
                returnType = returnType,
                parameters = parameters,
                isExtern = false
            )
        )
    }

    private val fnPtrNames = mutableMapOf<String, Name>()
    private fun lowerType(type: Type): CNode = when (type) {
        is Type.AssociatedTypeRef,
        is Type.Application,
        is Type.Closure,
        is Type.Error,
        is Type.GenericInstance,
        is Type.Param,
        is Type.Select,
        is Type.TypeFunction,
        -> requireUnreachable()

        is Type.Array -> TODO()
        Type.Bool -> CNode.Raw("bool")
        is Type.Constructor -> CNode.Raw(type.name.c())
        is Type.FloatingPoint -> lowerFloatingPointType(type)
        is Type.FunctionPtr -> {
            val key = type.from.joinToString(",", "(", ")") { lowerType(it).toPPNode().prettyPrint() } + ":" + lowerType(type.to).toPPNode().prettyPrint()
            val name = fnPtrNames.getOrPut(key) {
                val name = Name("_hds_fnptr${nextId()}")
                declarations.add(
                    CNode.TypedefFnPtr(
                        name = name,
                        returnType = lowerType(type.to),
                        parameters = type.from.map { lowerType(it) }
                    )
                )
                name
            }
            CNode.Raw(name.c())
        }
        is Type.Integral -> CNode.Raw(lowerIntegralType(type))
        is Type.Ptr -> CNode.PtrType(lowerType(type.to), isConst = !type.isMutable)
        is Type.Ref -> TODO()
        is Type.Size -> CNode.Raw("size_t")
        is Type.UntaggedUnion -> {
            val key = type.members.joinToString("|", prefix = "union[", postfix = "]") {
                lowerType(it).toPPNode().prettyPrint()
            }
            val name = unionNames.getOrPut(key) {
                val name = "_hds_union${nextId()}"
                declarations.add(
                    CNode.UnionDecl(
                        name = Name(name),
                        members = type.members.map { lowerType(it) }
                    )
                )
                Name(name)
            }
            CNode.Raw(name.c())
        }

        Type.Void -> CNode.Raw("void")
    }
    private var _nextId = 0
    private fun nextId(): Int {
        val id = _nextId
        _nextId += 1
        return id
    }

    private val unionNames = mutableMapOf<String, Name>()

    private fun lowerFloatingPointType(type: Type.FloatingPoint): CNode {
        return when (type.size) {
            32 -> CNode.Raw("float")
            64 -> CNode.Raw("double")
            else -> requireUnreachable { "Invalid floating point type size ${type.size}" }
        }
    }

    private fun lowerIntegralType(type: Type.Integral): String {
        val prefix = if (type.isSigned) "int" else "uint"
        return "${prefix}${type.size}_t"
    }

    private fun lowerStructInterface(definition: HIRDefinition.Struct): Unit {
        declarations.add(CNode.Raw("typedef struct ${definition.name.c()} ${definition.name.c()};"))
    }

    private fun lowerDefinitionImplementation(definition: HIRDefinition): Unit = when (definition) {
        is HIRDefinition.Const -> lowerConstDefinition(definition)
        is HIRDefinition.Function -> lowerFunctionImplementation(definition)
        is HIRDefinition.ExternConst -> {}
        is HIRDefinition.ExternFunction -> {}
        is HIRDefinition.Implementation -> requireUnreachable()
        is HIRDefinition.Struct -> lowerStructImplementation(definition)
    }

    private fun lowerConstDefinition(definition: HIRDefinition.Const) {
        val type = lowerType(definition.type)
        declarations.add(CNode.ConstDef(definition.name.c(), type, lowerExpression(definition.initializer)))
    }

    private fun lowerExpression(initializer: HIRExpression): CNode {
        TODO()
    }

    private fun lowerFunctionImplementation(def: HIRDefinition.Function) {
        val returnType = lowerType(def.returnType)
        val parameters = def.params.map { lowerType(it.type) }
        declarations.add(
            CNode.FnDefinition(
                name = def.name.c(),
                returnType = returnType,
                parameters = parameters,
                body = lowerBlocks(def.basicBlocks)

            )
        )
    }

    private fun lowerBlocks(blocks: List<HIRBlock>): CNode {
        TODO()
    }

    private fun lowerStructImplementation(definition: HIRDefinition.Struct) {
        declarations.add(
            CNode.StructDef(
                name = definition.name.c(),
                fields = definition.fields.map {
                    it.first.c() to lowerType(it.second)
                }
            )
        )
    }
}

fun Name.c(): String =
    text.replace("_", "_u_")
        .replace("$", "_d_")
        .replace("[", "_l_")
        .replace("]", "_r_")
        .replace(".", "_p_")
        .let { if (it[0].isDigit()) "_$it" else it }

fun QualifiedName.c(): String {
    return this.names.joinToString("_") {
        it.c()


    }

}

sealed interface CNode {
    data class Include(val path: String) : CNode
    data class Raw(val code: String) : CNode
    data class PtrType(val type: CNode, val isConst: Boolean) : CNode
    data class FnSignature(
        val name: String,
        val returnType: CNode,
        val parameters: List<CNode>,
        val isExtern: Boolean
    ) : CNode

    data class ExternConst(val name: String, val type: CNode) : CNode
    data class StructDef(val name: String, val fields: List<Pair<String, CNode>>) : CNode
    data class UnionDecl(val name: Name, val members: List<CNode>) : CNode
    data class TypedefFnPtr(
        val name: Name,
        val returnType: CNode,
        val parameters: List<CNode>
    ) : CNode
    data class FnDefinition(
        val name: String,
        val returnType: CNode,
        val parameters: List<CNode>,
        val body: CNode
    ) : CNode
    data class ConstDef(val name: String, val type: CNode, val initializer: CNode) : CNode
}

fun HIRDefinition.interfaceSortOrder(): Int {
    return when (this) {
        is HIRDefinition.Struct -> 0
        is HIRDefinition.Implementation -> requireUnreachable { "Expected previous phases to eliminate implementations" }
        is HIRDefinition.ExternConst -> 1
        is HIRDefinition.ExternFunction -> 2
        is HIRDefinition.Const -> 3
        is HIRDefinition.Function -> 4
    }
}