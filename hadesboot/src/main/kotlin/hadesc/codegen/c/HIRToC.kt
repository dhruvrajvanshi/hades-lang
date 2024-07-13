package hadesc.codegen.c

import hadesboot.prettyprint.PPNode
import hadesboot.prettyprint.prettyPrint
import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.hir.*
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.mutPtr

class HIRToC(
    private val hirModule: HIRModule,
    private val enableDebug: Boolean
) {
    private val declarations = mutableListOf<CNode>(
        CNode.Raw(
            """
            #include <stddef.h>
            #include <stdbool.h>
            #include <stdint.h>
            #include <stdalign.h>
        """.trimIndent()
        )
    )

    fun lower(): String {
        for (definition in hirModule.definitions.sortedBy { it.interfaceSortOrder() }) {
            lowerDefinitionInterface(definition)
        }
        for (definition in hirModule.definitions.sortedBy { it.definitionSortOrder() }) {
            lowerDefinitionImplementation(definition)
        }
        declarations.add(
            CNode.Raw(
                """
                int main() {
                    hades_u_main();
                    return 0;
                }
            """.trimIndent()
            )
        )
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

    private val externNames = mutableMapOf<QualifiedName, String>()
    private fun lowerExternConstInterface(definition: HIRDefinition.ExternConst) {
        val type = lowerType(definition.type)
        declarations.add(CNode.ExternConst(definition.externName.text, type))
        externNames[definition.name] = definition.externName.text
    }

    private fun lowerExternFunctionInterface(definition: HIRDefinition.ExternFunction) {
        val returnType = lowerType(definition.returnType)
        val parameters = definition.params.map { lowerType(it) }
        declarations.add(
            CNode.FnSignature(
                name = definition.externName.text,
                returnType = returnType,
                parameters = parameters,
                isExtern = true
            )
        )
        externNames[definition.name] = definition.externName.text
    }

    private fun lowerFunctionInterface(definition: HIRDefinition.Function) {
        val returnType = lowerType(definition.returnType)
        val parameters = definition.params.map { lowerType(it.type) }
        val name = lowerMainFnNameIfRequired(definition.name)
        declarations.add(
            CNode.FnSignature(
                name = name.c(),
                returnType = returnType,
                parameters = parameters,
                isExtern = false
            )
        )
    }

    private fun lowerMainFnNameIfRequired(name: QualifiedName): QualifiedName {
        return if (name.names.map { it.text } == listOf("main")) {
            QualifiedName(listOf(Name("hades_main")))
        } else {
            name
        }
    }

    private val fnPtrNames = mutableMapOf<String, Name>()
    private fun lowerType(type: Type, indirect: Boolean = false): CNode = when (type) {
        is Type.AssociatedTypeRef,
        is Type.Application,
        is Type.Closure,
        is Type.Error,
        is Type.GenericInstance,
        is Type.Param,
        is Type.Select,
        is Type.ForAll,
        -> {
            requireUnreachable()
        }

        is Type.Array -> TODO()
        Type.Bool -> CNode.Raw("bool")
        is Type.Constructor -> {
            val structDecl = hirModule.findStructDefOrNull(type.name)
            if (structDecl != null && !indirect) {
                lowerStructImplementation(structDecl)
            }
            CNode.Raw(type.name.c())
        }

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
        is Type.Ptr -> CNode.PtrType(lowerType(type.to, indirect = true), isConst = false)
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
        is HIRExpression.GlobalRef -> {
            val extern = externNames[expr.name]
            if (extern != null) {
                CNode.Raw(extern)
            } else {
                CNode.Raw(lowerMainFnNameIfRequired(expr.name).c())
            }
        }

        is HIRConstant.BoolValue -> CNode.Raw(if (expr.value) "true" else "false")
        is HIRConstant.ByteString -> lowerByteString(expr)
        is HIRConstant.Error -> requireUnreachable()
        is HIRConstant.FloatValue -> CNode.Raw(expr.value.toString())
        is HIRConstant.GlobalFunctionRef -> CNode.Raw(lowerMainFnNameIfRequired(expr.name).c())
        is HIRConstant.IntValue -> CNode.Raw(expr.value.toString())
        is HIRConstant.NullPtr -> CNode.Raw("NULL")
        is HIRConstant.SizeOf -> CNode.Call(CNode.Raw("sizeof"), listOf(lowerType(expr.ofType)), semi = false)
        is HIRConstant.AlignOf -> CNode.Call(CNode.Raw("alignof"), listOf(lowerType(expr.ofType)), semi = false)
        is HIRConstant.StructValue -> lowerStructLiteral(expr)
        is HIRConstant.Void -> requireUnreachable()
        is HIRExpression.LocalRef -> CNode.Raw(expr.name.c())
        is HIRExpression.ParamRef -> CNode.Raw(expr.name.c())
        is HIRExpression.TraitMethodRef -> requireUnreachable()
    }

    private fun lowerStructLiteral(expr: HIRConstant.StructValue): CNode {
        val ty = lowerType(expr.type)
        return CNode.Cast(
            ty,
            CNode.BraceInitializedList(
                expr.values.map { lowerExpression(it) }
            )
        )
    }

    private fun lowerByteString(expr: HIRConstant.ByteString): CNode {
        return CNode.Raw('"' + expr.bytes.joinToString("") {
            it.escapeToStr()
        } + '"')
    }

    private fun lowerFunctionImplementation(def: HIRDefinition.Function) {
        val returnType = lowerType(def.returnType)
        val parameters = def.params.map { it.name.c() to lowerType(it.type) }
        declarations.add(
            CNode.FnDefinition(
                name = lowerMainFnNameIfRequired(def.name).c(),
                returnType = returnType,
                parameters = parameters,
                body = lowerFunctionBody(def.basicBlocks)

            )
        )
    }

    private var currentFnPrelude: MutableList<CNode>? = null
    private fun lowerFunctionBody(blocks: List<HIRBlock>): CNode.FunctionBody {
        val items = mutableListOf<CNode.LabeledStatements>()
        val prelude = mutableListOf<CNode>(
            CNode.LocalDecl(Name("\$hades_dummy").c(), CNode.Raw("void*"))
        )
        check(currentFnPrelude == null)
        currentFnPrelude = prelude

        for (block in blocks) {
            val stmts = mutableListOf<CNode>()
            for (statement in block.statements) {
                if (enableDebug) {
                    stmts.add(
                        CNode.Raw("\n#line ${statement.location.start.line} \"${statement.location.file}\"")
                    )
                }
                lowerStatement(statement, into = stmts)
            }
            items.add(CNode.LabeledStatements(block.name.c(), stmts))
        }
        check(currentFnPrelude === prelude)
        currentFnPrelude = null
        return CNode.FunctionBody(prelude, items)
    }

    private fun lowerStatement(statement: HIRStatement, into: MutableList<CNode>): Unit = when (statement) {
        is HIRStatement.AllocRef -> TODO()
        is HIRStatement.Alloca -> lowerAllocaStatement(statement, into)
        is HIRStatement.AllocateClosure -> TODO()
        is HIRStatement.BinOp -> lowerBinOp(statement, into)
        is HIRStatement.Call -> lowerCallStatement(statement, into)
        is HIRStatement.GetStructField -> lowerGetStructField(statement, into)
        is HIRStatement.GetStructFieldPointer -> lowerGetStructFieldPtr(statement, into)
        is HIRStatement.IntegerConvert -> lowerIntegerConvert(statement, into)
        is HIRStatement.InvokeClosure -> requireUnreachable()
        is HIRStatement.Jump -> TODO()
        is HIRStatement.Load -> lowerLoadStatement(statement, into)
        is HIRStatement.LoadRefField -> TODO()
        is HIRStatement.MatchInt -> TODO()
        is HIRStatement.Memcpy -> TODO()
        is HIRStatement.Move -> TODO()
        is HIRStatement.Not -> lowerNotStatement(statement, into)
        is HIRStatement.PointerCast -> lowerPointerCast(statement, into)
        is HIRStatement.PtrToInt -> lowerPtrToInt(statement, into)
        is HIRStatement.IntToPtr -> lowerIntToPtr(statement, into)
        is HIRStatement.Return -> lowerReturnStatement(statement, into)
        is HIRStatement.Store -> lowerStoreStatement(statement, into)
        is HIRStatement.StoreRefField -> TODO()
        is HIRStatement.SwitchInt -> lowerSwitchInt(statement, into)
        is HIRStatement.TypeApplication -> TODO()
        is HIRStatement.While -> TODO()
    }

    private fun lowerBinOp(statement: HIRStatement.BinOp, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.type),
            value = CNode.RawPP(
                PPNode.Nodes(
                    lowerExpression(statement.lhs).toPPNode(),
                    PPNode.Text(" "),
                    PPNode.Text(statement.operator.toC()),
                    PPNode.Text(" "),
                    lowerExpression(statement.rhs).toPPNode()
                )
            )
        )
    }

    private fun lowerNotStatement(statement: HIRStatement.Not, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.expression.type),
            value = CNode.Prefix("!", lowerExpression(statement.expression))
        )
    }

    private fun lowerIntToPtr(statement: HIRStatement.IntToPtr, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.type),
            value = CNode.Cast(
                lowerType(statement.type),
                lowerExpression(statement.expression)
            )
        )
    }

    private fun lowerPtrToInt(statement: HIRStatement.PtrToInt, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.type),
            value = CNode.Cast(
                lowerType(statement.type),
                lowerExpression(statement.expression)
            )
        )
    }

    private fun lowerIntegerConvert(statement: HIRStatement.IntegerConvert, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.type),
            value = CNode.Cast(
                lowerType(statement.type),
                lowerExpression(statement.value)
            )
        )
    }

    private fun lowerSwitchInt(statement: HIRStatement.SwitchInt, into: MutableList<CNode>) {
        val cases = buildList {
            addAll(statement.cases.map {
                CNode.SwitchCase(
                    lowerExpression(it.value),
                    CNode.Goto(it.block.c()),
                )
            })
            add(CNode.DefaultCase(CNode.Goto(statement.otherwise.c())))
        }
        into.add(
            CNode.Switch(
                lowerExpression(statement.condition),
                cases
            )
        )
    }

    private fun lowerLoadStatement(statement: HIRStatement.Load, into: MutableList<CNode>) {
        if (statement.type == Type.Void) {
            return
        }
        val name = statement.name.c()
        addDeclAssign(
            into,
            name,
            type = lowerType(statement.type),
            value = CNode.Deref(
                lowerExpression(statement.ptr),
            )
        )
    }

    private fun addDeclAssign(
        into: MutableList<CNode>,
        name: String,
        type: CNode,
        value: CNode,
    ) {
        if (type == CNode.Raw("void")) {
            into.add(value)
        } else {
            val prelude = checkNotNull(currentFnPrelude)
            prelude.add(CNode.LocalDecl(name, type))
            into.add(CNode.Assign(CNode.Raw(name), value))
        }
    }

    private fun lowerPointerCast(statement: HIRStatement.PointerCast, into: MutableList<CNode>) {
        val toType = lowerType(statement.toPointerOfType.mutPtr())
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = toType,
            value = CNode.RawPP(

                PPNode.Nodes(
                    PPNode.Text("("),
                    toType.toPPNode(),
                    PPNode.Text(")"),
                    lowerExpression(statement.value).toPPNode()
                )
            ),
        )
    }

    private fun lowerGetStructFieldPtr(statement: HIRStatement.GetStructFieldPointer, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.type),
            value = CNode.AddressOf(CNode.Arrow(lowerExpression(statement.lhs), statement.memberName.c()))
        )
    }

    private fun lowerGetStructField(statement: HIRStatement.GetStructField, into: MutableList<CNode>) {
        addDeclAssign(
            into,
            name = statement.name.c(),
            type = lowerType(statement.type),
            value = CNode.Dot(lowerExpression(statement.lhs), statement.fieldName.c())
        )
    }

    private fun lowerReturnStatement(statement: HIRStatement.Return, into: MutableList<CNode>) {
        if (statement.expression.type is Type.Void) {
            into.add(CNode.Raw("return;"))
        } else {
            into.add(CNode.Return(lowerExpression((statement.expression))))
        }
    }

    private fun lowerStoreStatement(statement: HIRStatement.Store, into: MutableList<CNode>) {
        if (statement.value.type is Type.Void) {
            return
        }
        val ptr = lowerExpression(statement.ptr)
        val value = lowerExpression(statement.value)
        into.add(CNode.Assign(CNode.Prefix("*", ptr), value))
    }

    private fun lowerCallStatement(statement: HIRStatement.Call, into: MutableList<CNode>) {
        // FIXME: This should be handled upstream in HIRGen
        val args = statement.args.map { lowerExpression(it) }
        // Struct initializer
        if (statement.callee is HIRExpression.GlobalRef && hirModule.findStructDefOrNull(statement.callee.name) != null) {
            addDeclAssign(
                into,
                name = statement.name.c(),
                type = lowerType(statement.resultType),
                value = CNode.Cast(lowerType(statement.resultType), CNode.BraceInitializedList(args))
            )
            return
        }
        val call = CNode.Call(target = lowerExpression(statement.callee), args = args, semi = true)
        if (statement.resultType is Type.Void) {
            into.add(call)
        } else {
            addDeclAssign(
                into,
                name = statement.name.c(),
                type = lowerType(statement.resultType),
                value = call
            )
        }

    }

    private fun lowerAllocaStatement(statement: HIRStatement.Alloca, into: MutableList<CNode>) {
        if (statement.type is Type.Void) {
            return
        }
        val valueName = QualifiedName(listOf(statement.name, Name("value"))).c()
        val prelude = checkNotNull(currentFnPrelude)
        prelude.add(CNode.LocalDecl(valueName, lowerType(statement.type)))
        prelude.add(CNode.LocalDecl(statement.name.c(), lowerType(statement.type.mutPtr())))
        into.add(CNode.Assign(CNode.Raw(statement.name.c()), CNode.Raw("&$valueName")))
    }

    private val loweredStructImplSet = mutableSetOf<QualifiedName>()
    private fun lowerStructImplementation(definition: HIRDefinition.Struct) {
        if (definition.name in loweredStructImplSet) {
            return
        }
        loweredStructImplSet.add(definition.name)
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

private fun HIRDefinition.definitionSortOrder(): Int = when (this) {
    is HIRDefinition.Struct -> 0
    is HIRDefinition.ExternConst -> 1
    is HIRDefinition.ExternFunction -> 2
    is HIRDefinition.Const -> 3
    is HIRDefinition.Function -> 4
    is HIRDefinition.Implementation -> requireUnreachable()
}

private val reservedCIdentifiers = setOf(
    "bool",
    "break",
)
fun Name.c(): String {
    var text = this.text
    if (text in reservedCIdentifiers) {
        text = "\$$text"
    }
    return text.replace("_", "_u_")
        .replace("$", "_d_")
        .replace("[", "_l_")
        .replace("]", "_r_")
        .replace(".", "_p_")
        .replace("*", "_s_")
        .replace(",", "_c_")
        .replace(" ", "_ss_")
        .let { if (it[0].isDigit()) "_$it" else it }
}

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
        val parameters: List<Pair<String, CNode>>,
        val body: FunctionBody
    ) : CNode

    data class ConstDef(val name: String, val type: CNode, val initializer: CNode) : CNode
    data class FunctionBody(val prelude: List<CNode>, val items: List<LabeledStatements>) : CNode
    data class LocalDecl(val name: String, val type: CNode) : CNode
    data class Assign(val target: CNode, val value: CNode) : CNode
    data class Dot(val lhs: CNode, val rhs: String) : CNode
    data class Arrow(val lhs: CNode, val rhs: String) : CNode
    data class AddressOf(val target: CNode) : CNode
    data class Call(val target: CNode, val args: List<CNode>, val semi: Boolean) : CNode
    data class Return(val value: CNode) : CNode
    data class Prefix(val op: String, val value: CNode) : CNode
    data class RawPP(val node: PPNode) : CNode
    data class Deref(val node: CNode) : CNode
    data class LabeledStatements(
        val label: String,
        val statements: List<CNode>
    ) : CNode

    data class Switch(val expr: CNode, val cases: List<CNode>) : CNode
    data class SwitchCase(val value: CNode, val body: CNode) : CNode
    data class DefaultCase(val body: CNode) : CNode
    data class Goto(val label: String) : CNode
    data class Cast(val toType: CNode, val value: CNode) : CNode
    data class BraceInitializedList(val values: List<CNode>) : CNode
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
    '"'.code == toInt() -> "\\\""
    toInt() < 127
    -> Char(toInt()).toString()

    else ->
        "\\x${toInt().toString(16).padStart(2, '0')}"
}

fun BinaryOperator.toC(): String = when (this) {
    BinaryOperator.PLUS -> "+"
    BinaryOperator.MINUS -> "-"
    BinaryOperator.TIMES -> "*"
    BinaryOperator.DIV -> "/"
    BinaryOperator.REM -> "%"
    BinaryOperator.AND -> "&&"
    BinaryOperator.OR -> "||"
    BinaryOperator.EQUALS -> "=="
    BinaryOperator.NOT_EQUALS -> "!="
    BinaryOperator.GREATER_THAN -> ">"
    BinaryOperator.GREATER_THAN_EQUAL -> ">="
    BinaryOperator.LESS_THAN -> "<"
    BinaryOperator.LESS_THAN_EQUAL -> "<="
}