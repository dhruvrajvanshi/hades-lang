package hadesc.codegen.c

import hadesboot.prettyprint.prettyPrint
import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.hir.*
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr

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
            val key = type.from.joinToString(",", "(", ")") {
                lowerType(it).toPPNode().prettyPrint()
            } + ":" + lowerType(type.to).toPPNode().prettyPrint()
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

    private fun lowerExpression(expr: HIRExpression): CNode = when (expr) {
        is HIRExpression.GlobalRef -> CNode.Raw(expr.name.c())
        is HIRConstant.AlignOf -> TODO()
        is HIRConstant.BoolValue -> TODO()
        is HIRConstant.ByteString -> lowerByteString(expr)
        is HIRConstant.Error -> TODO()
        is HIRConstant.FloatValue -> TODO()
        is HIRConstant.GlobalFunctionRef -> TODO()
        is HIRConstant.IntValue -> CNode.Raw(expr.value.toString())
        is HIRConstant.NullPtr -> TODO()
        is HIRConstant.SizeOf -> TODO()
        is HIRConstant.StructValue -> TODO()
        is HIRConstant.Void -> TODO()
        is HIRExpression.LocalRef -> TODO()
        is HIRExpression.ParamRef -> TODO()
        is HIRExpression.TraitMethodRef -> TODO()
    }

    private fun lowerByteString(expr: HIRConstant.ByteString): CNode {
        return CNode.Raw('"' + expr.bytes.joinToString("") {
            it.escapeToStr()
        } + '"')
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
        val items = mutableListOf<CNode>()
        for (block in blocks) {
            items.add(CNode.Raw("${block.name.text}:"))
            for (statement in block.statements) {
                lowerStatement(statement, into = items)
            }
        }
        return CNode.Block(items)
    }

    private fun lowerStatement(statement: HIRStatement, into: MutableList<CNode>): Unit = when (statement) {
        is HIRStatement.AllocRef -> TODO()
        is HIRStatement.Alloca -> lowerAllocaStatement(statement, into)
        is HIRStatement.AllocateClosure -> TODO()
        is HIRStatement.BinOp -> TODO()
        is HIRStatement.Call -> lowerCallStatement(statement, into)
        is HIRStatement.GetStructField -> TODO()
        is HIRStatement.GetStructFieldPointer -> TODO()
        is HIRStatement.IntToPtr -> TODO()
        is HIRStatement.IntegerConvert -> TODO()
        is HIRStatement.InvokeClosure -> TODO()
        is HIRStatement.Jump -> TODO()
        is HIRStatement.Load -> TODO()
        is HIRStatement.LoadRefField -> TODO()
        is HIRStatement.MatchInt -> TODO()
        is HIRStatement.Memcpy -> TODO()
        is HIRStatement.Move -> TODO()
        is HIRStatement.Not -> TODO()
        is HIRStatement.PointerCast -> TODO()
        is HIRStatement.PtrToInt -> TODO()
        is HIRStatement.Return -> lowerReturnStatement(statement, into)
        is HIRStatement.Store -> lowerStoreStatement(statement)
        is HIRStatement.StoreRefField -> TODO()
        is HIRStatement.SwitchInt -> TODO()
        is HIRStatement.TypeApplication -> TODO()
        is HIRStatement.While -> TODO()
    }

    private fun lowerReturnStatement(statement: HIRStatement.Return, into: MutableList<CNode>) {
        if (statement.expression.type is Type.Void) {
            into.add(CNode.Raw("return;"))
        } else {
            into.add(CNode.Return(lowerExpression((statement.expression))))
        }
    }

    private fun lowerStoreStatement(statement: HIRStatement.Store) {
        if (statement.value.type is Type.Void) {
            return
        }
        TODO()
    }

    private fun lowerCallStatement(statement: HIRStatement.Call, into: MutableList<CNode>) {
        val args = statement.args.map { lowerExpression(it) }
        into.add(CNode.Call(target = lowerExpression(statement.callee), args = args))
    }

    private fun lowerAllocaStatement(statement: HIRStatement.Alloca, into: MutableList<CNode>) {
        if (statement.type is Type.Void) {
            return
        }
        val valueName = QualifiedName(listOf(statement.name, Name("value"))).c()
        into.add(CNode.LocalDecl(valueName, lowerType(statement.type)))
        into.add(CNode.LocalDecl(statement.name.c(), lowerType(statement.type.mutPtr())))
        into.add(CNode.Assign(CNode.Raw(statement.name.c()), CNode.Raw("&$valueName")))
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
    data class Block(val items: List<CNode>) : CNode
    data class LocalDecl(val name: String, val type: CNode) : CNode
    data class Assign(val target: CNode, val value: CNode) : CNode
    data class Call(val target: CNode, val args: List<CNode>) : CNode
    data class Return(val value: CNode) : CNode
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

fun Byte.escapeToStr(): String = when {
    '\\'.code == toInt() -> "\\\\"
    '\n'.code == toInt() -> "\\n"
    '\r'.code == toInt() -> "\\r"
    '\t'.code == toInt() -> "\\t"
    toInt() < 127
        -> Char(toInt()).toString()
    else ->
        "\\x${toInt().toString(16).padStart(2, '0')}"
}