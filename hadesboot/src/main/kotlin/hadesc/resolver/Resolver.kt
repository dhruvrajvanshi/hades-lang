package hadesc.resolver

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.frontend.PropertyBinding
import hadesc.location.HasLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName

class Resolver(private val ctx: Context) {
    private val sourceFileScopes = mutableMapOf<SourcePath, MutableList<ScopeTree>>()
    private val sourceFiles = mutableMapOf<SourcePath, SourceFile>()

    fun resolve(ident: Identifier): Binding? {
        val scopeStack = getScopeStack(ident)
        return resolveInScopeStack(ident, scopeStack)
    }

    fun resolveTypeVariable(ident: Identifier): TypeBinding? {
        val scopeStack = getScopeStack(ident)
        return findTypeInScopeStack(ident, scopeStack)
    }

    private fun resolveInScopeStack(ident: Identifier, scopeStack: ScopeStack): Binding? {
        for (scopeNode in scopeStack) {
            val binding = findInScope(ident, scopeNode)
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

    private fun findTypeInScope(ident: Identifier, scopeNode: ScopeTree): TypeBinding? = when (scopeNode) {
        is ScopeTree.FunctionDef -> {
            findTypeInFunctionDef(ident, scopeNode.declaration)
        }
        is ScopeTree.SourceFile -> {
            findTypeInSourceFile(ident, scopeNode.sourceFile)
        }
        is ScopeTree.Block -> null
        is ScopeTree.Struct -> {
            val param = scopeNode.declaration.typeParams?.findLast {
                it.binder.identifier.name == ident.name
            }
            when {
                param != null -> {
                    TypeBinding.TypeParam(param.binder, param.bound)
                }
                ident.name == scopeNode.declaration.binder.identifier.name -> {
                    TypeBinding.Struct(scopeNode.declaration)
                }
                else -> {
                    null
                }
            }
        }
        is ScopeTree.Enum -> {
            val param = scopeNode.declaration.typeParams?.find {
                it.binder.identifier.name == ident.name
            }
            when {
                param != null -> TypeBinding.TypeParam(param.binder, param.bound)
                ident.name == scopeNode.declaration.name.identifier.name -> {
                    TypeBinding.Enum(scopeNode.declaration)
                }
                else -> null
            }
        }
        is ScopeTree.MatchArm -> null
        is ScopeTree.TypeAlias -> {
            val param = scopeNode.declaration.typeParams?.find {
                it.binder.identifier.name == ident.name
            }
            if (param != null) TypeBinding.TypeParam(param.binder, param.bound) else null
        }
    }

    private fun findTypeInFunctionDef(ident: Identifier, declaration: Declaration.FunctionDef): TypeBinding? {
        val typeParams = declaration.typeParams ?: return null
        typeParams.forEach {
            if (it.binder.identifier.name == ident.name) {
                return TypeBinding.TypeParam(it.binder, it.bound)
            }
        }
        return null
    }

    private fun findTypeInSourceFile(ident: Identifier, sourceFile: SourceFile): TypeBinding? {
        for (declaration in sourceFile.declarations) {
            val binding = if (declaration is Declaration.Struct && declaration.binder.identifier.name == ident.name) {
                TypeBinding.Struct(declaration)
            } else if (declaration is Declaration.Enum && declaration.name.identifier.name == ident.name) {
                TypeBinding.Enum(declaration)
            } else if (declaration is Declaration.TypeAlias && declaration.name.identifier.name == ident.name) {
                TypeBinding.TypeAlias(declaration)
            } else {
                null
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findInScope(ident: Identifier, scope: ScopeTree): Binding? = when (scope) {
        is ScopeTree.FunctionDef -> findInFunctionDef(ident, scope)
        is ScopeTree.SourceFile -> findInSourceFile(ident.name, scope.sourceFile)
        is ScopeTree.Block -> findInBlock(ident, scope)
        is ScopeTree.Struct -> null
        is ScopeTree.Enum -> null
        is ScopeTree.MatchArm -> findInMatchArm(ident, scope)
        is ScopeTree.TypeAlias -> null
    }

    private fun findInMatchArm(ident: Identifier, scope: ScopeTree.MatchArm): Binding? {
        return findInPattern(ident, scope.arm.pattern)
    }

    private fun findInPattern(ident: Identifier, pattern: Pattern): Binding? = when (pattern) {
        is Pattern.DotName -> {
            var binding: Binding? = null
            for (param in pattern.params) {
                val found = findInPattern(ident, param)
                if (found != null) {
                    binding = found
                }
            }
            binding
        }
        is Pattern.Name -> {
            if (ident.name == pattern.binder.identifier.name) {
                Binding.Pattern(pattern)
            } else {
                null
            }
        }
        is Pattern.Else -> null
    }

    private fun findInBlock(ident: Identifier, scope: ScopeTree.Block): Binding? {
        for (member in scope.block.members) {
            val binding = when (member) {
                is Block.Member.Expression -> null
                is Block.Member.Statement -> when (member.statement) {
                    is Statement.Return -> null
                    is Statement.Val -> if (ident.name == member.statement.binder.identifier.name) {
                        Binding.ValBinding(member.statement)
                    } else {
                        null
                    }
                    is Statement.Error -> null
                    is Statement.While -> null
                    is Statement.If -> null
                    is Statement.LocalAssignment -> null
                    is Statement.MemberAssignment -> null
                    is Statement.PointerAssignment -> null
                    is Statement.Defer -> null
                }
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findInSourceFile(name: Name, sourceFile: SourceFile): Binding? {
        for (declaration in sourceFile.declarations) {
            val binding = when (declaration) {
                is Declaration.Error -> null
                is Declaration.ImportAs -> null
                is Declaration.FunctionDef -> {
                    if (declaration.name.identifier.name == name)
                        Binding.GlobalFunction(declaration)
                    else null
                }
                is Declaration.ExternFunctionDef -> {
                    if (declaration.binder.identifier.name == name) {
                        Binding.ExternFunction(declaration)
                    } else {
                        null
                    }
                }
                is Declaration.Struct -> {
                    if (declaration.binder.identifier.name == name) {
                        Binding.Struct(declaration)
                    } else {
                        null
                    }
                }
                is Declaration.ConstDefinition -> {
                    if (declaration.name.identifier.name == name) {
                        Binding.GlobalConst(declaration)
                    } else {
                        null
                    }
                }
                is Declaration.Enum -> null
                is Declaration.TypeAlias -> null
                is Declaration.ExtensionDef -> null
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findInFunctionDef(ident: Identifier, scope: ScopeTree.FunctionDef): Binding? {
        var index = -1
        for (param in scope.declaration.params) {
            index++
            if (param.binder.identifier.name == ident.name) {
                return Binding.FunctionParam(index, scope.declaration)
            }
        }
        return if (
            scope.declaration.typeParams == null &&
            ident.name == scope.declaration.name.identifier.name
        ) {
            Binding.GlobalFunction(scope.declaration)
        } else {
            null
        }
    }

    private fun sourceFileOf(node: HasLocation): SourceFile {
        return requireNotNull(sourceFiles[node.location.file])
    }

    private fun getScopeStack(node: HasLocation): ScopeStack {
        val scopes = sourceFileScopes
            .getOrDefault(node.location.file, emptyList<ScopeTree>())
            .sortedByDescending { it.location }
            .filter { it.location contains node }

        return ScopeStack(scopes)
    }

    fun qualifiedStructName(declaration: Declaration.Struct): QualifiedName {
        return sourceFileOf(declaration).moduleName.append(declaration.binder.identifier.name)
    }

    fun onParseMatchArm(arm: Expression.Match.Arm) {
        addScopeNode(arm.location.file, ScopeTree.MatchArm(arm))
    }

    fun onParseDeclaration(declaration: Declaration) {
        when (declaration) {
            is Declaration.FunctionDef -> {
                addScopeNode(
                    declaration.location.file,
                    ScopeTree.FunctionDef(declaration)
                )
            }
            is Declaration.Struct -> {
                addScopeNode(
                    declaration.location.file,
                    ScopeTree.Struct(declaration)
                )
            }
            is Declaration.Enum -> {
                addScopeNode(declaration.location.file, ScopeTree.Enum(declaration))
            }
            is Declaration.TypeAlias -> {
                addScopeNode(declaration.location.file, ScopeTree.TypeAlias(declaration))
            }
            else -> {}
        }
    }

    fun onParseSourceFile(sourceFile: SourceFile) {
        sourceFiles[sourceFile.location.file] = sourceFile
        addScopeNode(sourceFile.location.file, ScopeTree.SourceFile(sourceFile))
    }

    fun onParseBlock(block: Block) {
        addScopeNode(block.location.file, ScopeTree.Block(block))
    }

    private fun addScopeNode(file: SourcePath, scopeNode: ScopeTree) {
        sourceFileScopes
            .computeIfAbsent(file) { mutableListOf() }
            .add(scopeNode)
    }

    fun resolveModuleProperty(expression: Expression.Property): Binding? {
        val scopeStack = getScopeStack(expression.lhs)
        // TODO: Handle chained property calls
        if (expression.lhs !is Expression.Var) {
            return null
        }
        for (scope in scopeStack.scopes) {
            val binding: Binding? = when (scope) {
                is ScopeTree.FunctionDef -> null
                is ScopeTree.Block -> null // Blocks can't have imports yet
                is ScopeTree.Struct -> null
                is ScopeTree.SourceFile -> {
                    var binding: Binding? = null
                    for (declaration in scope.sourceFile.declarations) {
                        binding = when (declaration) {
                            is Declaration.Error -> null
                            is Declaration.ImportAs -> if (declaration.asName.identifier.name == expression.lhs.name.name) {
                                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                                if (sourceFile != null) {
                                    findInSourceFile(expression.property.name, sourceFile)
                                } else {
                                    null
                                }
                            } else {
                                null
                            }
                            is Declaration.FunctionDef -> null
                            is Declaration.ExternFunctionDef -> null
                            is Declaration.Struct -> null
                            is Declaration.ConstDefinition -> {
                                null
                            }
                            is Declaration.Enum -> {
                                if (declaration.name.identifier.name == expression.lhs.name.name) {
                                    declaration.cases.indexOfFirst {
                                        it.name.identifier.name == expression.property.name
                                    }.let { case ->
                                        if (case > -1) {
                                            Binding.EnumCaseConstructor(declaration, case)
                                        } else {
                                            null
                                        }
                                    }
                                } else {
                                    null
                                }
                            }
                            is Declaration.TypeAlias -> null
                            is Declaration.ExtensionDef -> null
                        }
                        if (binding != null) {
                            break
                        }
                    }
                    binding
                }
                is ScopeTree.MatchArm -> null
                is ScopeTree.TypeAlias -> null
                is ScopeTree.Enum -> null
            }
            if (binding != null) {
                return binding
            }

        }
        return null
    }

    fun resolveQualifiedType(path: QualifiedPath): TypeBinding? {
        require(path.identifiers.size == 2)
        val sourceFile = sourceFileOf(path)
        for (decl in sourceFile.declarations) {
            if (decl is Declaration.ImportAs && decl.asName.identifier.name == path.identifiers.first().name) {
                val importedSourceFile = ctx.resolveSourceFile(decl.modulePath) ?: return null
                return findTypeInSourceFile(path.identifiers.last(), importedSourceFile)
            }
        }
        return null
    }

    fun resolveDeclaration(qualifiedName: QualifiedName): Declaration? {
        val modulePath = QualifiedName(qualifiedName.names.dropLast(1))
        val declName = qualifiedName.names.last()
        val sourceFile = ctx.resolveSourceFile(modulePath) ?: return null
        for (decl in sourceFile.declarations) {
            val match = exhaustive(when(decl) {
                is Declaration.Error -> false
                is Declaration.ImportAs -> decl.asName.identifier.name == declName
                is Declaration.FunctionDef -> decl.name.identifier.name == declName
                is Declaration.ConstDefinition -> decl.name.identifier.name == declName
                is Declaration.ExternFunctionDef -> decl.binder.identifier.name == declName
                is Declaration.Struct -> decl.binder.identifier.name == declName
                is Declaration.Enum -> decl.name.identifier.name == declName
                is Declaration.TypeAlias -> decl.name.identifier.name == declName
                is Declaration.ExtensionDef -> false
            })
            if (match) {
                return decl
            }
        }
        return null
    }

    fun resolveDeclaration(path: QualifiedPath): Declaration? {
        if (path.identifiers.size == 1) {
            val name = path.identifiers.first()
            return findDeclarationOf(sourceFileOf(name), name)
        } else {
            require(path.identifiers.size == 2)
            val moduleName = path.identifiers.first()
            val sourceFile = sourceFileOf(moduleName)
            for (declaration in sourceFile.declarations) {
                if (declaration is Declaration.ImportAs && declaration.asName.identifier.name == moduleName.name) {
                    val name = path.identifiers[1]
                    val file = ctx.resolveSourceFile(declaration.modulePath) ?: return null
                    return findDeclarationOf(file, name)
                }
            }
            return null
        }

    }

    private fun findDeclarationOf(sourceFile: SourceFile, name: Identifier): Declaration? {
        for (declaration in sourceFile.declarations) {
            val decl = when (declaration) {
                is Declaration.Error -> null
                is Declaration.ImportAs -> null
                is Declaration.FunctionDef -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else null
                is Declaration.ConstDefinition -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is Declaration.ExternFunctionDef -> if (declaration.binder.identifier.name == name.name) {
                    declaration
                } else null
                is Declaration.Struct -> if (declaration.binder.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is Declaration.Enum -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is Declaration.TypeAlias -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is Declaration.ExtensionDef -> null
            }
            if (decl != null) {
                return decl
            }
        }
        return null

    }

    fun getEnclosingFunction(node: HasLocation): Declaration.FunctionDef? {
        for (scopeNode in getScopeStack(node)) {
            if (scopeNode is ScopeTree.FunctionDef) {
                return scopeNode.declaration
            }
        }
        return null
    }

    fun resolveGlobalName(binder: Binder): QualifiedName {
        return sourceFileOf(binder).moduleName.append(binder.identifier.name)
    }

    fun extensionDefsInScope(node: HasLocation): Sequence<Declaration.ExtensionDef> = sequence {
        val declarations = sourceFileOf(node).declarations
        yieldAll(extensionDefsInDeclarations(declarations, includeImports = true))
    }

    private fun extensionDefsInDeclarations(declarations: List<Declaration>, includeImports: Boolean): Sequence<Declaration.ExtensionDef> = sequence<Declaration.ExtensionDef> {
        for (declaration in declarations) {
            if (includeImports && declaration is Declaration.ImportAs) {
                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (sourceFile != null) {
                    yieldAll(extensionDefsInDeclarations(
                        sourceFile.declarations,
                        // extensions are not transitively included
                        includeImports = false
                    ))
                }
            }
            if (declaration !is Declaration.ExtensionDef) {
                continue
            }
            yield(declaration as Declaration.ExtensionDef)
        }
    }
}
