package hadesc.resolver

import hadesc.ast.*
import hadesc.context.Context
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
        val param: Param,
        val kind: Declaration.Kind.FunctionDef
    ) : ValueBinding() {
        init {
            assert(qualifiedName.size == 1)
        }
    }

    data class ImportAs(
        override val qualifiedName: QualifiedName,
        val aliasedModule: QualifiedName,
        val kind: Declaration.Kind.ImportAs
    ) : ValueBinding()

    data class ValBinding(
        override val qualifiedName: QualifiedName,
        val statement: Statement,
        val kind: Statement.Kind.Val
    ) : ValueBinding()

    data class Struct(
        override val qualifiedName: QualifiedName,
        val kind: Declaration.Kind.Struct
    ) : ValueBinding()
}

sealed class TypeBinding {
    data class FunctionDefTypeParam(
        val def: Declaration.Kind.FunctionDef,
        val binder: Binder,
        val paramIndex: Int
    ) : TypeBinding()
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
    data class Struct(
        val declaration: Declaration,
        val kind: Declaration.Kind.Struct
    ) : ScopeNode()

    val location
        get(): SourceLocation = when (this) {
            is FunctionDef -> SourceLocation.between(kind.params.firstOrNull() ?: kind.body, kind.body)
            is SourceFile -> sourceFile.location
            is Block -> block.location
            is Struct -> declaration.location
        }
}

data class ScopeStack(val scopes: List<ScopeNode>) : Iterable<ScopeNode> {
    val sourceFile: SourceFile

    init {
        val sourceFileScopeNode = scopes.last()
        if (sourceFileScopeNode !is ScopeNode.SourceFile) {
            throw AssertionError("Expected a sourcefile at the end of scope stack")
        }
        sourceFile = sourceFileScopeNode.sourceFile
    }

    override fun iterator(): Iterator<ScopeNode> {
        return scopes.iterator()
    }

}

class Resolver(val ctx: Context) {
    private val sourceFileScopes = mutableMapOf<SourcePath, MutableList<ScopeNode>>()
    private val valueBindings = mutableMapOf<QualifiedName, ValueBinding>()

    fun getBinding(ident: Identifier): ValueBinding {
        val scopeStack = getScopeStack(ident)
        // TODO: remove moduleName param becuase it can be queried
        // from the scope stack
        return findInScopeStack(ident, scopeStack.sourceFile.moduleName, scopeStack)
    }

    fun getTypeBinding(ident: Identifier): TypeBinding {
        val scopeStack = getScopeStack(ident)
        return findTypeBindingInScopeStack(ident, scopeStack)
    }

    private fun findTypeBindingInScopeStack(ident: Identifier, scopeStack: ScopeStack): TypeBinding {
        for (scope in scopeStack) {
            val binding = when (scope) {
                is ScopeNode.FunctionDef -> findTypeBindingInFunctionDef(ident, scope.kind)
                is ScopeNode.SourceFile -> null
                is ScopeNode.Block -> null
                is ScopeNode.Struct -> TODO()
            }

            if (binding != null) {
                return binding
            }
        }
        TODO("${ident.location}: Unbound type variable: $ident")
    }

    private fun findTypeBindingInFunctionDef(
        ident: Identifier,
        kind: Declaration.Kind.FunctionDef
    ): TypeBinding? {
        var index = -1
        for (typeParam in kind.typeParams) {
            index++
            if (typeParam.binder.identifier.name == ident.name) {
                return TypeBinding.FunctionDefTypeParam(kind, typeParam.binder, index)
            }
        }
        return null
    }

    private fun getScopeStack(ident: Identifier): ScopeStack {
        val scopes = sourceFileScopes
            .getOrDefault(ident.location.file, emptyList<ScopeNode>())
            .filter { it.location contains ident }
            .sortedByDescending { it.location }

        return ScopeStack(scopes)
    }


    private fun findInScopeStack(ident: Identifier, parentName: QualifiedName, scopeStack: ScopeStack): ValueBinding {
        val binding = findInScopeStackHelper(ident, parentName, scopeStack)
        valueBindings[binding.qualifiedName] = binding
        return binding
    }

    private fun findInScopeStackHelper(
        ident: Identifier,
        parentName: QualifiedName,
        scopes: ScopeStack
    ): ValueBinding {
        for (scope in scopes) {
            val binding = when (scope) {
                is ScopeNode.FunctionDef -> findInFunctionDef(parentName, ident, scope.declaration, scope.kind)
                is ScopeNode.SourceFile -> findInSourceFile(ident, scope.sourceFile)
                is ScopeNode.Block -> findInBlock(ident, scope.block)
                is ScopeNode.Struct -> TODO()
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
                            pathToQualifiedName(declaration.kind.modulePath),
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
                is Declaration.Kind.Struct -> {
                    if (declaration.kind.binder.identifier.name == ident.name) {
                        val qualifiedName = sourceFileModuleName.append(declaration.kind.binder.identifier.name)
                        val binding = ValueBinding.Struct(
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

    private fun pathToQualifiedName(path: QualifiedPath): QualifiedName = ctx.qualifiedPathToName(path)

    private fun findInFunctionDef(
        parentName: QualifiedName,
        ident: Identifier,
        declaration: Declaration,
        kind: Declaration.Kind.FunctionDef
    ): ValueBinding? {
        for (param in kind.params) {
            if (param.binder.identifier.name == ident.name) {
                val name = param.binder.identifier.name
                return ValueBinding.FunctionParam(
                    QualifiedName(listOf(name)),
                    declaration,
                    param,
                    kind
                )
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
                is Block.Member.Statement -> findInStatement(ident, member.statement)
            }

            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findInStatement(ident: Identifier, statement: Statement): ValueBinding? = when (statement.kind) {
        is Statement.Kind.Val -> {
            if (ident.name == statement.kind.binder.identifier.name) {
                // TODO: If rhs is a reference to a global, then we should
                // recursively resolve that here and give the root binding
                // instead of this alias
                ValueBinding.ValBinding(
                    QualifiedName(listOf(ident.name)),
                    statement,
                    statement.kind
                )
            } else {
                null
            }
        }
        else -> null
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

    fun resolveModuleMember(qualifiedName: QualifiedName, propertyName: Identifier): ValueBinding {
        return findInSourceFile(propertyName, ctx.resolveSourceFile(qualifiedName)) ?: TODO()
    }
}