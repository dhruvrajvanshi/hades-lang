package hadesc.resolver

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import org.jetbrains.annotations.Contract

sealed class ValueBinding {
    data class GlobalFunction(
        val qualifiedName: QualifiedName,
        val declaration: Declaration.FunctionDef
    ) : ValueBinding()

    data class ExternFunction(
        val qualifiedName: QualifiedName,
        val declaration: Declaration.ExternFunctionDef
    ) : ValueBinding()

    data class FunctionParam(
        val name: Name,
        val index: Int,
        val param: Param,
        val declaration: Declaration.FunctionDef
    ) : ValueBinding()

    data class ValBinding(
        val name: Name,
        val statement: Statement.Val
    ) : ValueBinding()

    data class Struct(
        val qualifiedName: QualifiedName,
        val declaration: Declaration.Struct
    ) : ValueBinding()

    data class GlobalConst(
        val qualifiedName: QualifiedName,
        val declaration: Declaration.ConstDefinition
    ) : ValueBinding()
}

sealed class TypeBinding {
    data class Struct(
        val declaration: Declaration.Struct
    ) : TypeBinding()

    data class TypeParam(val binder: Binder) : TypeBinding()
}

sealed class ScopeNode {
    // Update function getEnclosingDeclaration when adding a new declaration scope
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
            is FunctionDef -> declaration.location
            is SourceFile -> sourceFile.location
            is Block -> block.location
            is Struct -> declaration.location
        }
}

/**
 * A stack of scope, starting from narrow to wide
 */
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
    private val sourceFiles = mutableMapOf<SourcePath, SourceFile>()

    fun resolve(ident: Identifier): ValueBinding? {
        val scopeStack = getScopeStack(ident)
        return findInScopeStack(ident, scopeStack)
    }

    fun resolveTypeVariable(ident: Identifier): TypeBinding? {
        val scopeStack = getScopeStack(ident)
        return findTypeInScopeStack(ident, scopeStack)
    }

    private fun findInScopeStack(ident: Identifier, scopeStack: ScopeStack): ValueBinding? {
        for (scopeNode in scopeStack) {
            val binding = shallowFindInScope(ident, scopeNode)
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findTypeInScopeStack(ident: Identifier, scopeStack: ScopeStack): TypeBinding? {
        for (scopeNode in scopeStack) {
            val binding = findTypeInScope(ident, scopeNode)
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findTypeInScope(ident: Identifier, scopeNode: ScopeNode): TypeBinding? = when (scopeNode) {
        is ScopeNode.FunctionDef -> {
            findTypeInFunctionDef(ident, scopeNode.declaration)
        }
        is ScopeNode.SourceFile -> {
            findTypeInSourceFile(ident, scopeNode.sourceFile)
        }
        is ScopeNode.Block -> null
        is ScopeNode.Struct -> {
            val param = scopeNode.declaration.typeParams?.findLast {
                it.binder.identifier.name == ident.name
            }
            when {
                param != null -> {
                    TypeBinding.TypeParam(param.binder)
                }
                ident.name == scopeNode.declaration.binder.identifier.name -> {
                    TypeBinding.Struct(scopeNode.declaration)
                }
                else -> {
                    null
                }
            }
        }
    }

    private fun findTypeInFunctionDef(ident: Identifier, declaration: Declaration.FunctionDef): TypeBinding? {
        val typeParams = declaration.typeParams ?: return null
        typeParams.forEach {
            if (it.binder.identifier.name == ident.name) {
                return TypeBinding.TypeParam(it.binder)
            }
        }
        return null
    }

    private fun findTypeInSourceFile(ident: Identifier, sourceFile: SourceFile): TypeBinding.Struct? {
        for (declaration in sourceFile.declarations) {
            val binding = if (declaration is Declaration.Struct && declaration.binder.identifier.name == ident.name) {
                TypeBinding.Struct(declaration)
            } else {
                null
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun shallowFindInScope(ident: Identifier, scope: ScopeNode): ValueBinding? = when (scope) {
        is ScopeNode.FunctionDef -> shallowFindInFunction(ident, scope)
        is ScopeNode.SourceFile -> shallowFindInSourceFile(ident, scope.sourceFile)
        is ScopeNode.Block -> shallowFindInBlock(ident, scope)
        is ScopeNode.Struct -> null
    }

    private fun shallowFindInBlock(ident: Identifier, scope: ScopeNode.Block): ValueBinding? {
        for (member in scope.block.members) {
            val binding = when (member) {
                is Block.Member.Expression -> null
                is Block.Member.Statement -> when (member.statement) {
                    is Statement.Return -> null
                    is Statement.Val -> if (ident.name == member.statement.binder.identifier.name) {
                        ValueBinding.ValBinding(ident.name, member.statement)
                    } else {
                        null
                    }
                    is Statement.Error -> null
                    is Statement.While -> null
                    is Statement.If -> null
                }
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun shallowFindInSourceFile(ident: Identifier, sourceFile: SourceFile): ValueBinding? {
        for (declaration in sourceFile.declarations) {
            val binding = when (declaration) {
                is Declaration.Error -> null
                is Declaration.ImportAs -> null
                is Declaration.FunctionDef -> {
                    if (declaration.thisParam == null && declaration.name.identifier.name == ident.name) {
                        ValueBinding.GlobalFunction(
                            sourceFileOf(declaration).moduleName.append(ident.name),
                            declaration
                        )
                    } else {
                        null
                    }
                }
                is Declaration.ExternFunctionDef -> {
                    if (declaration.binder.identifier.name == ident.name) {
                        ValueBinding.ExternFunction(
                            sourceFileOf(declaration).moduleName.append(ident.name),
                            declaration
                        )
                    } else {
                        null
                    }
                }
                is Declaration.Struct -> {
                    if (declaration.binder.identifier.name == ident.name) {
                        ValueBinding.Struct(
                            sourceFileOf(declaration).moduleName.append(ident.name),
                            declaration
                        )
                    } else {
                        null
                    }
                }
                is Declaration.ConstDefinition -> {
                    if (declaration.name.identifier.name == ident.name) {
                        ValueBinding.GlobalConst(
                            sourceFileOf(declaration).moduleName.append(ident.name),
                            declaration
                        )
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

    private fun shallowFindInFunction(ident: Identifier, scope: ScopeNode.FunctionDef): ValueBinding? {
        var index = -1
        for (param in scope.declaration.params) {
            index++
            if (param.binder.identifier.name == ident.name) {
                return ValueBinding.FunctionParam(ident.name, index, param, scope.declaration)
            }
        }
        if (scope.declaration.typeParams == null && ident.name == scope.declaration.name.identifier.name) {
            val sourceFile = sourceFileOf(scope.declaration)
            return ValueBinding.GlobalFunction(
                sourceFile.moduleName.append(scope.declaration.name.identifier.name),
                scope.declaration
            )
        } else {
            return null
        }
    }

    private fun sourceFileOf(node: HasLocation): SourceFile {
        return requireNotNull(sourceFiles[node.location.file])
    }

    private fun getScopeStack(node: HasLocation): ScopeStack {
        val scopes = sourceFileScopes
            .getOrDefault(node.location.file, emptyList<ScopeNode>())
            .sortedByDescending { it.location }
            .filter { it.location contains node }

        return ScopeStack(scopes)
    }

    fun qualifiedStructName(declaration: Declaration.Struct): QualifiedName {
        return sourceFileOf(declaration).moduleName.append(declaration.binder.identifier.name)
    }

    fun onParseDeclaration(declaration: Declaration) {
        when (declaration) {
            is Declaration.FunctionDef -> {
                addScopeNode(
                    declaration.location.file,
                    ScopeNode.FunctionDef(declaration)
                )
            }
            is Declaration.Struct -> {
                addScopeNode(
                    declaration.location.file,
                    ScopeNode.Struct(declaration)
                )
            }
            else -> {}
        }
    }

    fun onParseSourceFile(sourceFile: SourceFile) {
        sourceFiles[sourceFile.location.file] = sourceFile
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

    fun getDeclarationContaining(node: HasLocation): Declaration {
        val sourceFile = requireNotNull(sourceFiles[node.location.file])
        for (declaration in sourceFile.declarations) {
            if (declaration.location contains node) {
                return declaration
            }
        }
        requireUnreachable()
    }

    fun resolveModuleProperty(expression: Expression.Property): ValueBinding? {
        val scopeStack = getScopeStack(expression.lhs)
        // TODO: Handle chained property calls
        if (expression.lhs !is Expression.Var) {
            return null
        }
        for (scope in scopeStack.scopes) {
            val binding: ValueBinding? = when (scope) {
                is ScopeNode.FunctionDef -> null
                is ScopeNode.Block -> null // Blocks can't have imports yet
                is ScopeNode.Struct -> null
                is ScopeNode.SourceFile -> {
                    var binding: ValueBinding? = null
                    for (declaration in scope.sourceFile.declarations) {
                        binding = when (declaration) {
                            is Declaration.Error -> null
                            is Declaration.ImportAs -> if (declaration.asName.identifier.name == expression.lhs.name.name) {
                                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                                shallowFindInSourceFile(expression.property, sourceFile)
                            } else {
                                null
                            }
                            is Declaration.FunctionDef -> null
                            is Declaration.ExternFunctionDef -> null
                            is Declaration.Struct -> null
                            is Declaration.ConstDefinition -> {
                                TODO()
                            }
                        }
                        if (binding != null) {
                            break
                        }
                    }
                    binding
                }
            }
            if (binding != null) {
                return binding
            }

        }
        return null
    }

    fun resolveThisParam(node: HasLocation): ThisParam? {
        val def = resolveThisBindingDef(node)
        require(def == null || def.thisParam != null)
        return def?.thisParam
    }

    private fun resolveThisBindingDef(node: HasLocation): Declaration.FunctionDef? {
        val scopeStack = getScopeStack(node)
        for (scope in scopeStack) {
            if (scope is ScopeNode.FunctionDef) {
                if (scope.declaration.thisParam != null) {
                    return scope.declaration
                }
            }
        }
        return null
    }

    fun extensionDefsInScope(node: HasLocation, name: Identifier) = sequence<Declaration.FunctionDef> {
        for (scope in getScopeStack(node)) {
            if (scope is ScopeNode.FunctionDef && isPossibleExtensionDef(scope.declaration, name)) {
                yield(scope.declaration)
            }
            fun extensionsInSourceFile(sourceFile: SourceFile): Sequence<Declaration.FunctionDef> = sequence {
                for (definition in sourceFile.declarations) {
                    if (definition is Declaration.FunctionDef && isPossibleExtensionDef(definition, name)) {
                        yield(definition as Declaration.FunctionDef)
                    }
                    if (definition is Declaration.ImportAs) {
                        yieldAll(extensionsInSourceFile(ctx.resolveSourceFile(definition.modulePath)))
                    }
                }
            }
            if (scope is ScopeNode.SourceFile) {
                yieldAll(extensionsInSourceFile(scope.sourceFile))
            }
        }
    }

    fun resolveQualifiedStructDef(path: QualifiedPath): Declaration.Struct? {
        require(path.identifiers.size == 2)
        val sourceFile = sourceFileOf(path)
        for (decl in sourceFile.declarations) {
            if (decl is Declaration.ImportAs && decl.asName.identifier.name == path.identifiers.first().name) {
                val importedSourceFile = ctx.resolveSourceFile(decl.modulePath)
                return findTypeInSourceFile(path.identifiers.last(), importedSourceFile)?.declaration
            }
        }
        return null
    }
}

@Contract(pure = true)
private fun isPossibleExtensionDef(def: Declaration.FunctionDef, property: Identifier): Boolean {
    return def.thisParam != null &&
            def.name.identifier.name == property.name
}