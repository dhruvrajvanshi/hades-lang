package hadesc.hir

import hadesc.unit

interface HIRModuleVisitor : HIRBlockVisitor {
    fun visitModule(module: HIRModule) {
        for (definition in module.definitions) {
            visitDefinition(definition)
        }
    }

    fun visitDefinition(definition: HIRDefinition) = when (definition) {
        is HIRDefinition.Const -> visitConstDef(definition)
        is HIRDefinition.ExternConst -> visitExternConstDef(definition)
        is HIRDefinition.ExternFunction -> visitExternFunctionDef(definition)
        is HIRDefinition.Function -> visitFunctionDef(definition)
        is HIRDefinition.Struct -> visitStructDef(definition)
    }

    fun visitStructDef(definition: HIRDefinition.Struct) {
        for (field in definition.fields) {
            visitType(field.second)
        }
    }

    fun visitFunctionDef(definition: HIRDefinition.Function) {
        for (basicBlock in definition.basicBlocks) {
            visitBlock(basicBlock)
        }
    }

    fun visitExternFunctionDef(definition: HIRDefinition.ExternFunction) = unit

    fun visitExternConstDef(definition: HIRDefinition.ExternConst) = unit

    fun visitConstDef(definition: HIRDefinition.Const) {
        visitExpression(definition.initializer)
    }
}
