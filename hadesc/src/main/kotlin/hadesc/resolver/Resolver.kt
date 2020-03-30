package hadesc.resolver

import hadesc.ast.Block
import hadesc.ast.Declaration
import hadesc.ast.Identifier
import hadesc.ast.SourceFile
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName

sealed class ValueBinding {
    abstract val qualifiedName: QualifiedName

    data class GlobalFunction(
        override val qualifiedName: QualifiedName,
        val kind: Declaration.Kind.FunctionDef
    ) : ValueBinding()

    data class ExternFunction(
        override val qualifiedName: QualifiedName,
        val kind: Declaration.Kind.ExternFunctionDef
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

    data class ImportAs(
        override val qualifiedName: QualifiedName,
        val kind: Declaration.Kind.ImportAs
    ) : ValueBinding()
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
            is FunctionDef -> SourceLocation.between(kind.params.firstOrNull() ?: kind.body, kind.body)
            is SourceFile -> sourceFile.location
            is Block -> block.location
        }
}

class Resolver {
    private val sourceFileScopes = mutableMapOf<SourcePath, MutableList<ScopeNode>>()
    private val valueBindings = mutableMapOf<QualifiedName, ValueBinding>()

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
        val binding = findInScopeStackHelper(ident, parentName, scopes)
        valueBindings[binding.qualifiedName] = binding
        return binding
    }

    private fun findInScopeStackHelper(
        ident: Identifier,
        parentName: QualifiedName,
        scopes: List<ScopeNode>
    ): ValueBinding {
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
        TODO("${ident.location}: Unbound variable ${ident.name.text} at")
    }

    fun findInSourceFile(ident: Identifier, sourceFile: SourceFile): ValueBinding? {
        val sourceFileModuleName = sourceFile.moduleName
        for (declaration in sourceFile.declarations) {
            val binding = when (declaration.kind) {
                Declaration.Kind.Error -> null
                is Declaration.Kind.ImportAs -> {
                    if (declaration.kind.asName.identifier.name == ident.name) {
                        ValueBinding.ImportAs(
                            QualifiedName(
                                listOf(
                                    declaration.kind.asName.identifier.name
                                )
                            ),
                            declaration.kind
                        )
                    } else {
                        null
                    }
                }
                is Declaration.Kind.FunctionDef -> {
                    if (declaration.kind.name.identifier.name == ident.name) {
                        val qualifiedName = sourceFileModuleName.append(declaration.kind.name.identifier.name)
                        val binding = ValueBinding.GlobalFunction(
                            qualifiedName,
                            declaration.kind
                        )
                        valueBindings[qualifiedName] = binding
                        binding
                    } else {
                        null
                    }
                }
                is Declaration.Kind.ExternFunctionDef -> {
                    if (declaration.kind.binder.identifier.name == ident.name) {
                        val qualifiedName = sourceFileModuleName.append(declaration.kind.binder.identifier.name)
                        val binding = ValueBinding.ExternFunction(
                            qualifiedName,
                            declaration.kind
                        )
                        valueBindings[qualifiedName] = binding
                        binding
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
        when (declaration.kind) {
            is Declaration.Kind.FunctionDef -> {
                addScopeNode(declaration.location.file, ScopeNode.FunctionDef(declaration, declaration.kind))
            }
            is Declaration.Kind.ExternFunctionDef -> {
            }

        }
    }

    fun onParseSourceFile(sourceFile: SourceFile) {
        addScopeNode(sourceFile.location.file, ScopeNode.SourceFile(sourceFile))
    }

    fun onParseBlock(block: Block) {
        addScopeNode(block.location.file, ScopeNode.Block(block))
    }

    private fun addScopeNode(file: SourcePath, scopeNode: ScopeNode) {
        sourceFileScopes
            .computeIfAbsent(file) { mutableListOf() }
            .add(scopeNode)
    }

    fun resolveQualifiedName(qualifiedName: QualifiedName): ValueBinding? {
        return valueBindings[qualifiedName]
    }
}