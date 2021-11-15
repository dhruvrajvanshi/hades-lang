package hadesc.frontend

import hadesc.analysis.infer
import hadesc.ast.*
import hadesc.context.Context
import hadesc.context.HasContext
import hadesc.diagnostics.Diagnostic
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
        is Declaration.ExternConst -> checkExternConstDef(declaration)
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.FunctionDef -> checkFunctionDef(declaration)
        is Declaration.ImplementationDef -> TODO()
        is Declaration.ImportAs -> checkImportAs(declaration)
        is Declaration.ImportMembers -> checkImportMembers(declaration)
        is Declaration.SealedType -> TODO()
        is Declaration.Struct -> TODO()
        is Declaration.TraitDef -> TODO()
        is Declaration.TypeAlias -> checkTypeAlias(declaration)
    }

    private fun checkTypeAlias(declaration: Declaration.TypeAlias) {
        declaration.rhs.toType()
    }

    private fun checkExternConstDef(declaration: Declaration.ExternConst) {
        declaration.type.toType()
    }

    private fun checkImportMembers(declaration: Declaration.ImportMembers) {
        val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
        if (sourceFile == null) {
            ctx.report(declaration.modulePath, Diagnostic.Kind.NoSuchModule)
            return
        }
        for (name in declaration.names) {
            val value = ctx.resolver.findInSourceFile(name.name, sourceFile)
            val type  = ctx.resolver.findTypeInSourceFile(name.identifier, sourceFile)
            val trait = ctx.resolver.findTraitInSourceFile(name.identifier, sourceFile)
            if (value == null && type == null && trait == null) {
                ctx.report(name, Diagnostic.Kind.UnboundVariable(name.name))
            }
        }
    }

    private fun checkImportAs(declaration: Declaration.ImportAs) {
        val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
        if (sourceFile == null) {
            ctx.diagnosticReporter.report(declaration.modulePath.location, Diagnostic.Kind.NoSuchModule)
        }
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        checkValueBinder(declaration.name)
        for (param in declaration.params) {
            param.annotation?.toType()
            if (param.annotation == null) {
                ctx.report(param, Diagnostic.Kind.MissingTypeAnnotation)
            }
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
            ctx.analyzer.assignBinderTypes(inferResult.binderTypes)
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