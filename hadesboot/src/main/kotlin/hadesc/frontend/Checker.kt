package hadesc.frontend

import hadesc.analysis.infer
import hadesc.ast.*
import hadesc.context.Context
import hadesc.context.HasContext
import hadesc.types.Type
import libhades.collections.Stack

class Checker(override val ctx: Context): HasContext {
    private val returnTypeStack = Stack<Type>()

    fun checkProgram() {
        ctx.forEachSourceFile { sourceFile ->
            checkSourceFile(sourceFile)
        }
    }

    private fun checkSourceFile(sourceFile: SourceFile) {
        sourceFile.declarations.forEach { declaration ->
            checkDeclaration(declaration)
        }
    }

    private fun checkDeclaration(declaration: Declaration) = when(declaration) {
        is Declaration.Error -> {}
        is Declaration.ConstDefinition -> TODO()
        is Declaration.ExtensionDef -> TODO()
        is Declaration.ExternConst -> TODO()
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.FunctionDef -> checkFunctionDef(declaration)
        is Declaration.ImplementationDef -> TODO()
        is Declaration.ImportAs -> TODO()
        is Declaration.ImportMembers -> TODO()
        is Declaration.SealedType -> TODO()
        is Declaration.Struct -> TODO()
        is Declaration.TraitDef -> TODO()
        is Declaration.TypeAlias -> TODO()
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        checkValueBinder(declaration.name)
        for (param in declaration.params) {
            param.annotation?.toType()
        }
        declaration.signature.whereClause?.let {
            checkWhereClause(it)
        }

        val returnType = declaration.signature.returnType.toType()
        check(returnTypeStack.isEmpty())
        returnTypeStack.push(returnType)
        checkBlock(declaration.body)
        returnTypeStack.pop()
        check(returnTypeStack.isEmpty())

    }

    private fun checkBlock(block: Block) {
        for (member in block.members) {
            val inferResult = infer(member, checkNotNull(returnTypeStack.peek()), ctx)
            ctx.analyzer.assignExpressionTypes(inferResult.expressionTypes)
        }
    }

    private fun checkWhereClause(it: WhereClause) {
        TODO()
    }

    private fun checkExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        declaration.paramTypes.map { it.toType() }
        declaration.returnType.toType()
    }

    private fun checkValueBinder(name: Binder) {
    }
}