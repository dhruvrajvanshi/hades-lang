package hadesc.resolver

import hadesc.ast.Block
import hadesc.ast.Declaration
import hadesc.ast.Identifier
import hadesc.ast.SourceFile
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName

sealed class ValueBinding {
    abstract val qualifiedName: QualifiedName

    data class GlobalFunction(
        override val qualifiedName: QualifiedName,
        val kind: Declaration.Kind.FunctionDef
    ) : ValueBinding()

    data class ExternFunction(
        override val qualifiedName: QualifiedName
    ) : ValueBinding()

    data class FunctionParam(
        override val qualifiedName: QualifiedName,
        val declaration: Declaration,
        val kind: Declaration.Kind.FunctionDef
    ) : ValueBinding() {
        init {
            assert(qualifiedName.size == 1)
        }
    }
}

sealed class ScopeNode {
    data class FunctionDef(
        val declaration: Declaration,
        val kind: Declaration.Kind.FunctionDef
    ) : ScopeNode()

    data class SourceFile(
        val sourceFile: hadesc.ast.SourceFile
    ) : ScopeNode()

    data class Block(val block: hadesc.ast.Block) : ScopeNode()

    val location
        get(): SourceLocation = when (this) {
            is FunctionDef -> declaration.location
            is SourceFile -> sourceFile.location
            is Block -> block.location
        }
}

class Resolver {
    private val log = logger()
    private val sourceFileScopes = mutableMapOf<SourcePath, MutableList<ScopeNode>>()

    fun getBinding(ident: Identifier): ValueBinding {
        val scopeStack = sourceFileScopes
            .getOrDefault(ident.location.file, emptyList<ScopeNode>())
            .filter { it.location contains ident }
            .sortedByDescending { it.location }
        val sourceFileScopeNode = scopeStack.last()
        if (sourceFileScopeNode !is ScopeNode.SourceFile) {
            throw AssertionError("Expected a sourcefile at the end of scope stack")
        }
        return findInScopeStack(ident, sourceFileScopeNode.sourceFile.moduleName, scopeStack)
    }

    private fun findInScopeStack(ident: Identifier, parentName: QualifiedName, scopes: List<ScopeNode>): ValueBinding {
        for (scope in scopes) {
            val binding = when (scope) {
                is ScopeNode.FunctionDef -> findInFunctionDef(parentName, ident, scope.declaration, scope.kind)
                is ScopeNode.SourceFile -> findInSourceFile(ident, scope.sourceFile)
                is ScopeNode.Block -> findInBlock(ident, scope.block)
            }
            if (binding != null) {
                return binding
            }
        }
        TODO("Unbound variable $ident at ${ident.location}")
    }

    private fun findInSourceFile(ident: Identifier, sourceFile: SourceFile): ValueBinding? {
        for (declaration in sourceFile.declarations) {
            val binding = when (declaration.kind) {
                Declaration.Kind.Error -> null
                is Declaration.Kind.ImportAs -> TODO()
                is Declaration.Kind.FunctionDef -> {
                    if (declaration.kind.name.identifier.name == ident.name) {
                        ValueBinding.GlobalFunction(
                            sourceFile.moduleName.append(declaration.kind.name.identifier.name),
                            declaration.kind
                        )
                    } else {
                        null
                    }
                }
                is Declaration.Kind.ExternFunctionDef -> {
                    if (declaration.kind.binder.identifier.name == ident.name) {
                        ValueBinding.ExternFunction(QualifiedName(listOf(declaration.kind.externName.name)))
                    } else {
                        null
                    }
                }
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findInFunctionDef(
        parentName: QualifiedName,
        ident: Identifier,
        declaration: Declaration,
        kind: Declaration.Kind.FunctionDef
    ): ValueBinding? {
        for (param in kind.params) {
            if (param.binder.identifier.name == ident.name) {
                val name = param.binder.identifier.name
                return ValueBinding.FunctionParam(QualifiedName(listOf(name)), declaration, kind)
            }
        }
        if (kind.name.identifier.name == ident.name) {
            return ValueBinding.GlobalFunction(parentName.append(kind.name.identifier.name), kind)
        }
        return null
    }

    private fun findInBlock(ident: Identifier, block: Block): ValueBinding? {
        for (member in block.members) {
            val binding: ValueBinding? = when (member) {
                is Block.Member.Expression -> null
                is Block.Member.Statement -> null
            }

            if (binding != null) {
                return binding
            }
        }
        return null
    }

    fun onParseDeclaration(declaration: Declaration) {
        log.debug("onParseDeclaration: ${locRange(declaration.location)}")
        when (declaration.kind) {
            is Declaration.Kind.FunctionDef -> {
                addScopeNode(declaration.location.file, ScopeNode.FunctionDef(declaration, declaration.kind))
            }
            is Declaration.Kind.ExternFunctionDef -> {
            }

        }
    }

    fun onParseSourceFile(sourceFile: SourceFile) {
        log.debug("onParseSourceFile: ${sourceFile.location.file}")
        addScopeNode(sourceFile.location.file, ScopeNode.SourceFile(sourceFile))
    }

    fun onParseBlock(block: Block) {
        log.debug("onParseBlock: ${locRange(block.location)}")
        addScopeNode(block.location.file, ScopeNode.Block(block))
    }

    private fun addScopeNode(file: SourcePath, scopeNode: ScopeNode) {
        sourceFileScopes
            .computeIfAbsent(file) { mutableListOf() }
            .add(scopeNode)
    }

    private fun locRange(location: SourceLocation) {
        log.debug("${location.file}: ${location.start} to ${location.stop}")
    }
}