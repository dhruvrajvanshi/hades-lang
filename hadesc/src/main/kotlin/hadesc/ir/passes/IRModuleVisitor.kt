package hadesc.ir.passes

import hadesc.ir.*

class IRModuleVisitor {
    fun visitModule(module: IRModule) {
        for (definition in module.definitions) {
            visitDefinition(definition)
        }
    }

    fun visitDefinition(definition: IRDefinition) = when (definition) {
        is IRFunctionDef -> visitFunctionDef(definition)
        is IRStructDef -> {
        }
        is IRExternFunctionDef -> {
        }
    }

    fun visitFunctionDef(definition: IRFunctionDef) {
        for (statement in definition.body) {
            visitStatement(statement)
        }
    }

    fun visitStatement(statement: IRStatement): Unit = when (statement) {
        is IRValStatement -> {
            visitExpression(statement.initializer)
        }
        is IRReturnStatement -> {
            visitExpression(statement.value)
        }
        is IRReturnVoidStatement -> {
        }
        is IRExpressionStatement -> {
            visitExpression(statement.expression)
        }
    }

    fun visitExpression(value: IRValue): Unit = when (value) {
        is IRCall -> {
            value.args.forEach { visitExpression(it) }
        }
        is IRBool -> {
        }
        is IRByteString -> {
        }
        is IRVariable -> {
        }
        is IRGetStructField -> {
            visitExpression(value.lhs)
        }
    }
}