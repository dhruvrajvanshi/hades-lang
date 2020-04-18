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
        for (statement in definition.body.statements) {
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
        IRReturnVoidStatement -> {
        }
        is IRExpression -> visitExpression(statement)
    }

    fun visitExpression(expression: IRExpression): Unit = when (expression) {
        is IRCallExpression -> {
            expression.args.forEach { visitExpression(it) }
        }
        is IRBool -> {
        }
        is IRByteString -> {
        }
        is IRVariable -> {
        }
        is IRGetStructField -> {
            visitExpression(expression.lhs)
        }
    }
}