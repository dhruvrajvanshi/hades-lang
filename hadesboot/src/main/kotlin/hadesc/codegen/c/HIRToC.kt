package hadesc.codegen.c

import hadesboot.prettyprint.prettyPrint
import hadesc.assertions.requireUnreachable
import hadesc.hir.HIRDefinition
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
        println(declarationsToPPNode(declarations).prettyPrint())
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
        is Type.FunctionPtr -> TODO()
        is Type.Integral -> CNode.Raw(lowerIntegralType(type))
        is Type.Ptr -> CNode.PtrType(lowerType(type.to), isConst = !type.isMutable)
        is Type.Ref -> TODO()
        is Type.Size -> CNode.Raw("size_t")
        is Type.UntaggedUnion -> TODO()
        Type.Void -> CNode.Raw("void")
    }

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
        is HIRDefinition.Const -> TODO()
        is HIRDefinition.ExternConst -> TODO()
        is HIRDefinition.ExternFunction -> TODO()
        is HIRDefinition.Function -> TODO()
        is HIRDefinition.Implementation -> TODO()
        is HIRDefinition.Struct -> TODO()
    }
}

fun QualifiedName.c(): String {
    return this.names.joinToString("_") {
        it.text.replace("_", "_u_")
            .replace("$", "_d_")
            .replace("[", "_l_")
            .replace("]", "_r_")
            .replace(".", "_p_")


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