package hadesc.resolver

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName

sealed class ThisBinding {
    data class Struct(val declaration: Declaration.Struct) : ThisBinding()
}

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

    data class StructField(
        val structDef: Declaration.Struct,
        val field: Declaration.Struct.Member.Field
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

    fun resolveThis(node: HasLocation): ThisBinding? {
        val scopeStack = getScopeStack(node)
        for (scope in scopeStack) {
            exhaustive(
                when (scope) {
                    is ScopeNode.FunctionDef -> {
                        // this is not bound in static declarations
                        // so we need to exit early so as to not
                        // get the enclosing struct/enum def
                        if (scope.declaration.flags.contains(DeclarationFlags.STATIC)) {
                            return null
                        } else {
                            null
                        }
                    }
                    is ScopeNode.SourceFile -> {
                    }
                    is ScopeNode.Block -> {
                    }
                    is ScopeNode.Struct -> {
                        return ThisBinding.Struct(scope.declaration)
                    }
                }
            )
        }
        return null
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
        var isIdentInStaticMethod = false
        for (scopeNode in scopeStack) {
            val isCurrentScopeNodeAStaticMethod = when (scopeNode) {
                is ScopeNode.FunctionDef -> {
                    DeclarationFlags.STATIC in scopeNode.declaration.flags
                }
                else -> false
            }
            if (isCurrentScopeNodeAStaticMethod) {
                isIdentInStaticMethod = true
            }
            val binding = findTypeInScope(ident, scopeNode, isIdentInStaticMethod)
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findTypeInScope(ident: Identifier, scopeNode: ScopeNode, isIdentInStaticMethod: Boolean): TypeBinding? =
        when (scopeNode) {
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
                    if (!isIdentInStaticMethod) {
                        TypeBinding.TypeParam(param.binder)
                    } else {
                        null
                    }
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
        val typeParams = declaration.typeParams
        if (typeParams == null) {
            return null
        }
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
        is ScopeNode.Struct -> shallowFindInStruct(ident, scope)
    }

    private fun shallowFindInStruct(ident: Identifier, scope: ScopeNode.Struct): ValueBinding? {
        for (member in scope.declaration.members) {
            val binding: ValueBinding? = when (member) {
                Declaration.Struct.Member.Error -> null
                is Declaration.Struct.Member.Field -> {
                    if (ident.name == member.binder.identifier.name) {
                        ValueBinding.StructField(scope.declaration, member)
                    } else {
                        null
                    }
                }
                is Declaration.Struct.Member.Method -> {
                    if (ident.name == member.function.name.identifier.name) {
                        ValueBinding.GlobalFunction(
                            sourceFileOf(member.function).moduleName
                                .append(scope.declaration.binder.identifier.name)
                                .append(member.function.name.identifier.name),
                            member.function
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
                    if (declaration.name.identifier.name == ident.name) {
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
        if (ident.name == scope.declaration.name.identifier.name) {
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


    private fun pathToQualifiedName(path: QualifiedPath): QualifiedName = ctx.qualifiedPathToName(path)

    private fun sourceFileModuleName(node: HasLocation): QualifiedName {
        return ctx.getSourceFileOf(node).moduleName
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
                for (member in declaration.members) {
                    if (member is Declaration.Struct.Member.Method) {
                        addScopeNode(
                            member.function.location.file,
                            ScopeNode.FunctionDef(member.function)
                        )
                    }
                }
            }

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
        val staticProperty = when (val lhsBinding = resolve(expression.lhs.name)) {
            is ValueBinding.GlobalFunction -> null
            is ValueBinding.ExternFunction -> null
            is ValueBinding.FunctionParam -> null
            is ValueBinding.ValBinding -> null
            is ValueBinding.Struct -> {
                findStaticMethod(lhsBinding.declaration, expression.property)
            }
            null -> null
            is ValueBinding.StructField -> null
        }
        if (staticProperty != null) {
            return staticProperty
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

    private fun findStaticMethod(declaration: Declaration.Struct, property: Identifier): ValueBinding? {
        for (member in declaration.members) {
            val binding = when (member) {
                Declaration.Struct.Member.Error -> null
                is Declaration.Struct.Member.Field -> null
                is Declaration.Struct.Member.Method -> {
                    if (member.function.flags.contains(DeclarationFlags.STATIC)
                        && member.function.name.identifier.name == property.name
                    ) {
                        ValueBinding.GlobalFunction(getQualifiedName(declaration.binder), member.function)
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

    fun getQualifiedName(binder: Binder): QualifiedName = when (val binding = resolve(binder.identifier)) {
        is ValueBinding.GlobalFunction -> TODO()
        is ValueBinding.ExternFunction -> TODO()
        is ValueBinding.Struct -> {
            val sourceFile = sourceFileOf(binder)
            sourceFile.moduleName.append(binder.identifier.name)
        }
        is ValueBinding.FunctionParam -> requireUnreachable()
        is ValueBinding.ValBinding -> requireUnreachable()
        null -> requireUnreachable()
        is ValueBinding.StructField -> requireUnreachable()
    }
}