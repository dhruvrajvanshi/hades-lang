package hadesc.resolver

import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName

sealed class ValueBinding {
    abstract val qualifiedName: QualifiedName

    data class GlobalFunction(
        override val qualifiedName: QualifiedName,
        val declaration: Declaration.FunctionDef
    ) : ValueBinding()

    data class ExternFunction(
        override val qualifiedName: QualifiedName,
        val declaration: Declaration.ExternFunctionDef
    ) : ValueBinding()

    data class FunctionParam(
        override val qualifiedName: QualifiedName,
        val param: Param,
        val declaration: Declaration.FunctionDef
    ) : ValueBinding() {
        init {
            require(qualifiedName.size == 1)
        }
    }

    data class ImportAs(
        override val qualifiedName: QualifiedName,
        val aliasedModule: QualifiedName,
        val declaration: Declaration.ImportAs
    ) : ValueBinding()

    data class ValBinding(
        override val qualifiedName: QualifiedName,
        val statement: Statement.Val
    ) : ValueBinding()

    data class Struct(
        override val qualifiedName: QualifiedName,
        val declaration: Declaration.Struct
    ) : ValueBinding()
}

sealed class TypeBinding {
    data class FunctionDefTypeParam(
        val def: Declaration.FunctionDef,
        val binder: Binder,
        val paramIndex: Int
    ) : TypeBinding()
}

sealed class ScopeNode {
    data class FunctionDef(
        val declaration: Declaration.FunctionDef
    ) : ScopeNode()

    data class SourceFile(
        val sourceFile: hadesc.ast.SourceFile
    ) : ScopeNode()

    data class Block(val block: hadesc.ast.Block) : ScopeNode()
    data class Struct(val declaration: Declaration.Struct) : ScopeNode()

    val location
        get(): SourceLocation = when (this) {
            is FunctionDef -> SourceLocation.between(
                declaration.params.firstOrNull() ?: declaration.returnType,
                declaration.body
            )
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
        return findInScopeStack(ident, scopeStack)
    }

    fun getBinding(binder: Binder): ValueBinding {
        val scope = findEnclosingScope(binder)
        val binding = findBindingInScope(binder.identifier, scope)
        return requireNotNull(binding)
    }

    fun getTypeBinding(ident: Identifier): TypeBinding {
        val scopeStack = getScopeStack(ident)
        return findTypeBindingInScopeStack(ident, scopeStack)
    }

    private fun findTypeBindingInScopeStack(ident: Identifier, scopeStack: ScopeStack): TypeBinding {
        for (scope in scopeStack) {
            val binding = when (scope) {
                is ScopeNode.FunctionDef -> findTypeBindingInFunctionDef(ident, scope.declaration)
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
        def: Declaration.FunctionDef
    ): TypeBinding? {
        var index = -1
        for (typeParam in def.typeParams) {
            index++
            if (typeParam.binder.identifier.name == ident.name) {
                return TypeBinding.FunctionDefTypeParam(def, typeParam.binder, index)
            }
        }
        return null
    }

    private fun findEnclosingScope(node: HasLocation): ScopeNode {
        val location = node.location
        return requireNotNull(sourceFileScopes.getOrDefault(location.file, emptyList<ScopeNode>())
            .sortedByDescending { it.location }
            .find { it.location contains node }) {
            "${node.location} not inside any scope"
        }

    }

    private fun getScopeStack(ident: Identifier): ScopeStack {
        val scopes = sourceFileScopes
            .getOrDefault(ident.location.file, emptyList<ScopeNode>())
            .filter { it.location contains ident }
            .sortedByDescending { it.location }

        return ScopeStack(scopes)
    }


    private fun findInScopeStack(ident: Identifier, scopeStack: ScopeStack): ValueBinding {
        val binding = findInScopeStackHelper(ident, scopeStack)
        valueBindings[binding.qualifiedName] = binding
        return binding
    }

    private fun findBindingInScope(ident: Identifier, scope: ScopeNode): ValueBinding? = when (scope) {
        is ScopeNode.FunctionDef -> findInFunctionDef(
            ident,
            scope.declaration
        )
        is ScopeNode.SourceFile -> findInSourceFile(ident, scope.sourceFile)
        is ScopeNode.Block -> findInBlock(ident, scope.block)
        is ScopeNode.Struct -> TODO()
    }

    private fun findInScopeStackHelper(
        ident: Identifier,
        scopes: ScopeStack
    ): ValueBinding {
        for (scope in scopes) {
            val binding = findBindingInScope(ident, scope)
            if (binding != null) {
                return binding
            }
        }
        TODO("${ident.location}: Unbound variable ${ident.name.text} at")
    }

    fun findInSourceFile(ident: Identifier, sourceFile: SourceFile): ValueBinding? {
        val sourceFileModuleName = sourceFile.moduleName
        for (declaration in sourceFile.declarations) {
            val binding = when (declaration) {
                is Declaration.Error -> null
                is Declaration.ImportAs -> {
                    if (declaration.asName.identifier.name == ident.name) {
                        ValueBinding.ImportAs(
                            QualifiedName(
                                listOf(
                                    declaration.asName.identifier.name
                                )
                            ),
                            pathToQualifiedName(declaration.modulePath),
                            declaration
                        )
                    } else {
                        null
                    }
                }
                is Declaration.FunctionDef -> {
                    if (declaration.name.identifier.name == ident.name) {
                        val qualifiedName = sourceFileModuleName.append(declaration.name.identifier.name)
                        val binding = ValueBinding.GlobalFunction(
                            qualifiedName,
                            declaration
                        )
                        valueBindings[qualifiedName] = binding
                        binding
                    } else {
                        null
                    }
                }
                is Declaration.ExternFunctionDef -> {
                    if (declaration.binder.identifier.name == ident.name) {
                        val qualifiedName = sourceFileModuleName.append(declaration.binder.identifier.name)
                        val binding = ValueBinding.ExternFunction(
                            QualifiedName(listOf(declaration.externName.name)),
                            declaration
                        )
                        valueBindings[qualifiedName] = binding
                        binding
                    } else {
                        null
                    }
                }
                is Declaration.Struct -> {
                    if (declaration.binder.identifier.name == ident.name) {
                        val qualifiedName = sourceFileModuleName.append(declaration.binder.identifier.name)
                        val binding = ValueBinding.Struct(
                            qualifiedName,
                            declaration
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
        ident: Identifier,
        declaration: Declaration.FunctionDef
    ): ValueBinding? {
        for (param in declaration.params) {
            if (param.binder.identifier.name == ident.name) {
                val name = param.binder.identifier.name
                return ValueBinding.FunctionParam(
                    QualifiedName(listOf(name)),
                    param,
                    declaration
                )
            }
        }
        if (declaration.name.identifier.name == ident.name) {
            return ValueBinding.GlobalFunction(
                sourceFileModuleName(declaration).append(declaration.name.identifier.name),
                declaration
            )
        }
        return null
    }

    private fun sourceFileModuleName(node: HasLocation): QualifiedName {
        return ctx.getSourceFileOf(node).moduleName
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

    private fun findInStatement(ident: Identifier, statement: Statement): ValueBinding? = when (statement) {
        is Statement.Val -> {
            if (ident.name == statement.binder.identifier.name) {
                // TODO: If rhs is a reference to a global, then we should
                // recursively resolve that here and give the root binding
                // instead of this alias
                ValueBinding.ValBinding(
                    QualifiedName(listOf(ident.name)),
                    statement
                )
            } else {
                null
            }
        }
        else -> null
    }

    fun onParseDeclaration(declaration: Declaration) {
        when (declaration) {
            is Declaration.FunctionDef -> {
                addScopeNode(
                    declaration.location.file,
                    ScopeNode.FunctionDef(declaration)
                )
            }
            is Declaration.ExternFunctionDef -> {
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