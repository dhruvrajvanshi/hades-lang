package hadesc.codegen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.ptr
import java.nio.charset.Charset
import java.nio.file.Paths

class HIRToC(
    private val ctx: Context,
    private val module: HIRModule
)  {
    private val cDecls = mutableListOf<CDecl>()
    fun run() {
        cDecls.apply {
            add(CDecl("#include <stdint.h>"))
            add(CDecl("#include <stdbool.h>"))
        }
        for (def in module.definitions.sortedBy {
            when (it) {
                is HIRDefinition.Struct -> 0
                is HIRDefinition.ExternFunction -> 1
                is HIRDefinition.ExternConst -> 1
                is HIRDefinition.Const -> 2
                is HIRDefinition.Function -> 3
                is HIRDefinition.Implementation -> requireUnreachable()
            }
        }) {
            visitDef(def)
        }
        val cSource = cDecls.joinToString("\n\n") { it.text }
        val outPath = ctx.target.output
        check(outPath != null)
        val cSourceName = outPath.fileName.toString() + ".c"
        val cSourcePath = Paths.get(".hades", cSourceName)

        Paths.get(".hades").toFile().mkdirs()

        cSourcePath.toFile().apply {
            writeText(cSource)
            appendText("\nint main() { hades_main(); return 0; }")
        }
        val commandParts = mutableListOf(
            "gcc",
            "-fsanitize=address",
            "-fsanitize=undefined",
            "-o",
            checkNotNull(ctx.target.output).toString(),
            cSourcePath.toString(),
        ).apply {
            addAll(ctx.options.cSources.map { it.toString() })
        }
        val builder = ProcessBuilder(commandParts)
        val process = builder
            .inheritIO()
            .start()
        val exitCode = process.waitFor()
        check(exitCode == 0) {
            "${commandParts.joinToString(" ")} exited with code $exitCode"
        }
    }

    private fun visitDef(def: HIRDefinition): Unit = when (def) {
        is HIRDefinition.Const -> visitConstDef(def)
        is HIRDefinition.ExternConst -> visitExternConst(def)
        is HIRDefinition.ExternFunction -> visitExternFunctionDef(def)
        is HIRDefinition.Function -> visitFunctionDef(def)
        is HIRDefinition.Implementation -> TODO()
        is HIRDefinition.Struct -> visitStructDef(def)
    }

    private fun visitConstDef(def: HIRDefinition.Const) {
        cDecls.add(
            CDecl("${def.type.lower()} ${def.name.c} = ${def.initializer.lower()};")
        )
    }

    private fun visitExternConst(def: HIRDefinition.ExternConst) {
        cDecls.add(
            CDecl("""
                extern ${def.type.lower()} ${def.externName.c};
            """.trimIndent())
        )
    }

    private fun visitFunctionDef(def: HIRDefinition.Function) {

        val params = def.params.joinToString(", ") { it.type.lower() + " " + it.name.c }

        val body = def.basicBlocks.joinToString("\n") {
            it.name.c + ":\n    " +  it.statements.joinToString("\n    ") { st -> st.lower() }
        }

        val name =
            if (def.name.mangle() == "main")
                "hades_main"
            else
                def.name.c
        cDecls.add(
            CDecl("${def.returnType.lower()} $name($params) {$body\n}")
        )
    }

    private fun HIRStatement.lower(): String = when(this) {
        is HIRStatement.Alloca -> lowerAlloca(this)
        is HIRStatement.AllocateClosure -> TODO()
        is HIRStatement.BinOp -> TODO()
        is HIRStatement.Call -> lowerCall(this)
        is HIRStatement.GetStructField -> TODO()
        is HIRStatement.GetStructFieldPointer -> TODO()
        is HIRStatement.IntToPtr -> TODO()
        is HIRStatement.IntegerConvert -> TODO()
        is HIRStatement.InvokeClosure -> TODO()
        is HIRStatement.Jump -> TODO()
        is HIRStatement.Load -> TODO()
        is HIRStatement.MatchInt -> TODO()
        is HIRStatement.Memcpy -> TODO()
        is HIRStatement.Move -> TODO()
        is HIRStatement.Not -> TODO()
        is HIRStatement.PointerCast -> TODO()
        is HIRStatement.PtrToInt -> TODO()
        is HIRStatement.Return -> lowerReturn(this)
        is HIRStatement.Store -> lowerStore(this)
        is HIRStatement.SwitchInt -> TODO()
        is HIRStatement.TypeApplication -> TODO()
        is HIRStatement.While -> TODO()
    }

    private fun lowerReturn(statement: HIRStatement.Return): String {
        if (statement.expression.type is Type.Void) {
            return "return;"
        }
        return "return ${statement.expression.lower()};"
    }

    private fun lowerStore(store: HIRStatement.Store): String {
        if (store.value.type is Type.Void) {
            if (store.value is HIRExpression.LocalRef) {
                return ""
            }
            return store.value.lower() + ";"
        }
        val lhs = "*" + store.ptr.lower()
        val rhs = store.value.lower()
        return "$lhs = $rhs;"
    }

    private fun lowerAlloca(alloca: HIRStatement.Alloca): String {
        if (alloca.type is Type.Void) {
            return ""
        }
        val tmpName = ctx.makeUniqueName("gen")
        return "${alloca.type.lower()} ${tmpName.c}; ${alloca.type.ptr(isMutable = true).lower()} ${alloca.name.c} = &${tmpName.c};"
    }

    private fun lowerCall(call: HIRStatement.Call): String {
        val args = call.args.joinToString(", ") { it.lower() }
        val prefix =
            if (call.resultType is Type.Void)
                ""
            else
                "${call.resultType.lower()} ${call.name.c} = "
        return "$prefix${call.callee.lower()}($args);"
    }

    private fun HIROperand.lower(): String = when(this) {
        is HIRExpression.GlobalRef -> {
            when (val global = checkNotNull(module.findGlobalDefinition(name))) {
                is HIRDefinition.Const -> global.name.c
                is HIRDefinition.ExternConst -> global.externName.text
                is HIRDefinition.ExternFunction -> global.externName.text
                is HIRDefinition.Function -> global.name.c
                is HIRDefinition.Implementation -> requireUnreachable()
                is HIRDefinition.Struct -> requireUnreachable()
            }
        }
        is HIRConstant.AlignOf -> TODO()
        is HIRConstant.BoolValue -> TODO()
        is HIRConstant.ByteString ->
            "\"${bytes.toString(charset = Charset.defaultCharset())
                .replace("\n", "\\n")
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
            }\""
        is HIRConstant.FloatValue -> TODO()
        is HIRConstant.IntValue -> "$value"
        is HIRConstant.NullPtr -> TODO()
        is HIRConstant.SizeOf -> TODO()
        is HIRConstant.Void -> requireUnreachable()
        is HIRExpression.LocalRef -> name.c
        is HIRExpression.ParamRef -> TODO()
        is HIRExpression.TraitMethodRef -> TODO()
    }

    private fun visitExternFunctionDef(def: HIRDefinition.ExternFunction) {
        val params = def.params.joinToString(", ") { it.lower() }
        cDecls.add(
            CDecl("""
                extern ${def.returnType.lower()} ${def.externName.text}($params);
            """.trimIndent())
        )
    }

    private fun visitStructDef(def: HIRDefinition.Struct) {
        check(def.typeParams == null)
        val fields =
            def.fields
                .joinToString(";\n") { "  ${it.second.lower()} ${it.first.c}" }
        cDecls.add(
            CDecl(
                """
                   typedef struct {
                   $fields
                   } ${def.name.c};
                """.trimIndent()
            )
        )
    }

    private fun Type.lower(): String = when(this) {
        is Type.AssociatedTypeRef,
        is Type.Error,
        is Type.GenericInstance,
        is Type.ParamRef,
        is Type.Select,
        is Type.TypeFunction,
        is Type.Application -> requireUnreachable()
        Type.Bool -> "bool"
        is Type.FloatingPoint -> when (size) {
            64 -> "double"
            32 -> "float"
            else -> requireUnreachable()
        }
        is Type.Integral -> {
            if (size == 8) {
                "char"
            } else {
                val prefix = if (isSigned) "" else "u"
                "${prefix}int${size}_t"
            }
        }
        is Type.Ptr -> {
            if (isMutable) {
                "${to.lower()}*"
            } else {
                "const ${to.lower()}*"
            }
        }
        is Type.Constructor -> name.c
        is Type.Function -> TODO()

        is Type.Size -> if (isSigned) "ssize_t" else "size_t"
        is Type.UntaggedUnion -> TODO()
        Type.Void -> "void"
    }

    private val QualifiedName.c get() =
        names
            .map { it.c }
            .joinToString(separator = "_p_") { it }
    private val Name.c get() =
        text
            .replace("_", "___")
            .replace(".", "_p_")
            .replace("$", "_d_")
            .replace("[", "_l_")
            .replace("]", "_r_")
}

data class CDecl(val text: String)