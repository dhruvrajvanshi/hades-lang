package hadesc.resolver

import hadesc.Name
import hadesc.ast.*
import hadesc.ast.Declaration.*
import hadesc.ast.Expression.*
import hadesc.context.SourceFileResolverCtx
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import llvm.makeList

class Resolver<Ctx>(private val ctx: Ctx) where Ctx: SourceFileResolverCtx {
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

    fun resolveTraitDef(name: Identifier): TraitDef? {
        val scopeStack = getScopeStack(name)
        for (scope in scopeStack.scopes) {
            val typeBinding = findTypeInScope(name, scope)
            if (typeBinding is TypeBinding.Trait) {
                return typeBinding.declaration
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
        val builtinType = when (ident.name.text) {
            "Int" -> Type.Integral(32, true)
            "Bool" -> Type.Bool
            "Byte" -> Type.Integral(8, false)
            "usize" -> Type.Size(isSigned = false)
            "isize" -> Type.Size(isSigned = true)
            "Size" -> Type.Size(isSigned = false)
            "Double" -> Type.FloatingPoint(64)
            "Void" -> Type.Void
            "u8" -> Type.Integral(8, false)
            "i8" -> Type.Integral(8, true)
            "u16" -> Type.Integral(16, false)
            "i16" -> Type.Integral(16, true)
            "u32" -> Type.Integral(32, false)
            "i32" -> Type.Integral(32, true)
            "u64" -> Type.Integral(64, false)
            "i64" -> Type.Integral(64, true)
            "f16" -> Type.FloatingPoint(16)
            "f32" -> Type.FloatingPoint(32)
            "f64" -> Type.FloatingPoint(64)
            else -> null
        }
        if (builtinType != null) {
            return TypeBinding.Builtin(builtinType)
        }
        return null
    }

    private fun findTypeInScope(ident: Identifier, scopeNode: ScopeTree): TypeBinding? = when (scopeNode) {
        is FunctionDef -> {
            findTypeInFunctionDef(ident, scopeNode)
        }
        is SourceFile -> {
            findTypeInSourceFile(ident, scopeNode)
        }
        is Block -> null
        is Struct -> {
            val param = scopeNode.typeParams?.findLast {
                it.binder.identifier.name == ident.name
            }
            when {
                param != null -> {
                    TypeBinding.TypeParam(param.binder)
                }
                ident.name == scopeNode.binder.identifier.name -> {
                    TypeBinding.Struct(scopeNode)
                }
                else -> {
                    null
                }
            }
        }
        is Declaration.Enum -> {
            val param = scopeNode.typeParams?.findLast {
                it.binder.identifier.name == ident.name
            }
            if (param != null) {
                TypeBinding.TypeParam(param.binder)
            } else {
                null
            }
        }
        is TypeAlias -> {
            val param = scopeNode.typeParams?.find {
                it.binder.identifier.name == ident.name
            }
            if (param != null) TypeBinding.TypeParam(param.binder) else null
        }
        is ExtensionDef -> {
            val param = scopeNode.typeParams?.find {
                it.binder.identifier.name == ident.name
            }
            if (param != null) TypeBinding.TypeParam(param.binder) else null
        }
        is TraitDef -> {
            if (scopeNode.name.identifier.name == ident.name) {
                TypeBinding.Trait(scopeNode)
            }

            val associatedType = scopeNode.members.filterIsInstance<TraitMember.AssociatedType>()
                .find { it.binder.name == ident.name }
            if (associatedType != null) {
                TypeBinding.AssociatedType(associatedType.binder)
            } else {
                scopeNode.params.find {
                    it.binder.identifier.name == ident.name
                }?.let { TypeBinding.TypeParam(it.binder) }
            }
        }
        is ImplementationDef -> {
            val aliasDef = scopeNode.body.filterIsInstance<TypeAlias>().find {
                it.name.name == ident.name
            }
            if (aliasDef != null) {
                TypeBinding.TypeAlias(aliasDef)
            } else {
                scopeNode.typeParams?.find {
                    it.binder.identifier.name == ident.name
                }?.let { TypeBinding.TypeParam(it.binder) }
            }
        }
        is Closure -> null
        is Match -> null
        is Match.Arm -> null
        is Statement.While -> null
    }

    private fun findTypeInFunctionDef(ident: Identifier, declaration: FunctionDef): TypeBinding? {
        val typeParams = declaration.typeParams ?: return null
        typeParams.forEach {
            if (it.binder.identifier.name == ident.name) {
                return TypeBinding.TypeParam(it.binder)
            }
        }
        return null
    }

    fun findTypeInSourceFile(ident: Identifier, sourceFile: SourceFile): TypeBinding? {
        for (declaration in sourceFile.declarations) {
            val binding = if (declaration is Struct && declaration.binder.identifier.name == ident.name) {
                TypeBinding.Struct(declaration)
            } else if (declaration is TypeAlias && declaration.name.identifier.name == ident.name) {
                TypeBinding.TypeAlias(declaration)
            } else if (declaration is TraitDef && declaration.name.identifier.name == ident.name) {
                TypeBinding.Trait(declaration)
            } else if (declaration is Declaration.Enum && declaration.name.identifier.name == ident.name) {
                TypeBinding.Enum(declaration)
            } else if (declaration is ImportMembers) {
                val binding = declaration.names.find { it.name == ident.name }
                val importedSourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (importedSourceFile != null && binding != null) {
                    findTypeInSourceFile(ident, importedSourceFile)
                } else {
                    null
                }
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
        is FunctionDef -> findInFunctionDef(ident, scope)
        is SourceFile -> findInSourceFile(ident.name, scope)
        is Block -> findInBlock(ident, scope)
        is Struct -> null
        is TypeAlias -> null
        is ExtensionDef -> null
        is TraitDef -> null
        is ImplementationDef -> null
        is Closure -> findInClosure(ident, scope)
        is Declaration.Enum -> null
        is Match -> null
        is Match.Arm -> findInMatchArm(ident, scope)
        is Statement.While -> findInBlock(ident, scope.body)
    }

    private fun findInMatchArm(ident: Identifier, scope: Match.Arm): Binding? {
        return findInPattern(ident, scope.pattern)
    }

    private fun findInPattern(ident: Identifier, pattern: Pattern): Binding? = when (pattern) {
        is Pattern.EnumCase ->
            pattern.args
                ?.asReversed() // last match wins
                ?.mapIndexedNotNull { indexFromBack, arg ->
                    val index = pattern.args.size - 1 - indexFromBack
                    when (arg) {
                        is Pattern.Val -> if (arg.binder.name == ident.name) {
                            Binding.MatchArmEnumCaseArg(pattern, index)
                        } else {
                            null
                        }
                        else -> null
                    }
                }
                ?.firstOrNull()
        is Pattern.IntLiteral -> null
        is Pattern.Val -> TODO()
        is Pattern.Wildcard -> null
    }

    private fun findInBlock(ident: Identifier, scope: Block): Binding? {
        for (member in scope.members) {
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

    fun findInSourceFile(name: Name, sourceFile: SourceFile): Binding? {
        for (declaration in sourceFile.declarations) {
            val binding = when (declaration) {
                is Declaration.Error -> null
                is ImportAs -> null
                is FunctionDef -> {
                    if (declaration.name.identifier.name == name) {
                        Binding.GlobalFunction(declaration)
                    } else {
                        null
                    }
                }
                is ExternFunctionDef -> {
                    if (declaration.binder.identifier.name == name) {
                        Binding.ExternFunction(declaration)
                    } else {
                        null
                    }
                }
                is Struct -> {
                    if (declaration.binder.identifier.name == name) {
                        Binding.Struct(declaration)
                    } else {
                        null
                    }
                }
                is ConstDefinition -> {
                    if (declaration.name.identifier.name == name) {
                        Binding.GlobalConst(declaration)
                    } else {
                        null
                    }
                }
                is ExternConst -> {
                    if (declaration.name.identifier.name == name) {
                        Binding.ExternConst(declaration)
                    } else {
                        null
                    }
                }
                is TypeAlias -> null
                is ExtensionDef -> null
                is TraitDef -> null
                is ImplementationDef -> null
                is ImportMembers -> {
                    val importedSourceFile = ctx.resolveSourceFile(declaration.modulePath)
                    if (importedSourceFile != null && declaration.names.any { it.name == name }) {
                        findInSourceFile(name, importedSourceFile)
                    } else {
                        null
                    }
                }
                is Declaration.Enum -> if (name == declaration.name.identifier.name) {
                    Binding.Enum(declaration)
                } else {
                    null
                }
            }
            if (binding != null) {
                return binding
            }
        }
        return null
    }

    private fun findInClosure(ident: Identifier, scope: Closure): Binding? {
        var index = -1
        for (param in scope.params) {
            index++
            if (param.binder.identifier.name == ident.name) {
                return Binding.ClosureParam(index, scope)
            }
        }
        return null
    }

    private fun findInFunctionDef(ident: Identifier, scope: FunctionDef): Binding? {
        var index = -1
        for (param in scope.params) {
            index++
            if (param.binder.identifier.name == ident.name) {
                return Binding.FunctionParam(index, scope)
            }
        }

        return if (
            scope.typeParams == null &&
            ident.name == scope.name.identifier.name
        ) {
            Binding.GlobalFunction(scope)
        } else {
            null
        }
    }

    private fun sourceFileOf(node: HasLocation): SourceFile {
        return requireNotNull(sourceFiles[node.location.file])
    }

    private fun getScopeStack(node: HasLocation): ScopeStack {
        val scopes = sourceFileScopes
            .getOrDefault(node.location.file, emptyList())
            .sortedByDescending { it.location }
            .filter { it.location.contains(node) }

        return ScopeStack(scopes)
    }

    fun qualifiedStructName(declaration: Struct): QualifiedName {
        return sourceFileOf(declaration).moduleName.append(declaration.binder.identifier.name)
    }

    fun qualifiedName(name: Binder): QualifiedName {
        return sourceFileOf(name).moduleName.append(name.identifier.name)
    }

    fun onParseClosure(closure: Closure) {
        addScopeNode(closure.location.file, closure)
    }

    fun onParseDeclaration(declaration: Declaration) {
        when (declaration) {
            is ScopeTree -> {
                addScopeNode(declaration.location.file, declaration)
            }
            else -> {}
        }
    }

    fun onParseSourceFile(sourceFile: SourceFile) {
        sourceFiles[sourceFile.location.file] = sourceFile
        addScopeNode(sourceFile.location.file, sourceFile)
    }

    fun onParseBlock(block: Block) {
        addScopeNode(block.location.file, block)
    }

    private fun addScopeNode(file: SourcePath, scopeNode: ScopeTree) {
        sourceFileScopes
            .computeIfAbsent(file) { mutableListOf() }
            .add(scopeNode)
    }

    fun onParseScopeNode(scopeNode: ScopeTree) {
        addScopeNode(scopeNode.location.file, scopeNode)
    }

    fun resolveModuleProperty(expression: Property): Binding? {
        val scopeStack = getScopeStack(expression.lhs)
        // TODO: Handle chained property calls
        if (expression.lhs !is Var) {
            return null
        }
        for (scope in scopeStack.scopes) {
            val binding: Binding? = when (scope) {
                is FunctionDef -> null
                is Block -> null // Blocks can't have imports yet
                is Struct -> null
                is Closure -> null
                is SourceFile -> {
                    var binding: Binding? = null
                    for (declaration in scope.declarations) {
                        binding = when (declaration) {
                            is ImportAs -> if (declaration.asName.identifier.name == expression.lhs.name.name) {
                                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                                if (sourceFile != null) {
                                    findInSourceFile(expression.property.name, sourceFile)
                                } else {
                                    null
                                }
                            } else {
                                null
                            }
                            else -> null
                        }
                        if (binding != null) {
                            break
                        }
                    }
                    binding
                }
                else -> null
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
            if (decl is ImportAs && decl.asName.identifier.name == path.identifiers.first().name) {
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
            val match = exhaustive(
                when (decl) {
                    is Declaration.Error -> false
                    is ImportAs -> decl.asName.identifier.name == declName
                    is FunctionDef -> decl.name.identifier.name == declName
                    is ConstDefinition -> decl.name.identifier.name == declName
                    is ExternFunctionDef -> decl.binder.identifier.name == declName
                    is Struct -> decl.binder.identifier.name == declName
                    is TypeAlias -> decl.name.identifier.name == declName
                    is ExtensionDef -> false
                    is TraitDef -> decl.name.identifier.name == declName
                    is ImplementationDef -> false
                    is ImportMembers -> false
                    is Declaration.Enum -> decl.name.identifier.name == declName
                    is ExternConst -> decl.name.identifier.name == declName
                }
            )
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
                if (declaration is ImportAs && declaration.asName.identifier.name == moduleName.name) {
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
                is ImportAs -> null
                is FunctionDef -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is ConstDefinition -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is ExternConst -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is ExternFunctionDef -> if (declaration.binder.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is Struct -> if (declaration.binder.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is TypeAlias -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is ExtensionDef -> null
                is TraitDef -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is ImplementationDef -> null
                is ImportMembers -> {
                    if (declaration.names.map { it.name }.contains(name.name)) {
                        val importedSourceFile = ctx.resolveSourceFile(declaration.modulePath)
                        if (importedSourceFile != null) {
                            findDeclarationOf(importedSourceFile, name)
                        } else {
                            null
                        }
                    } else {
                        null
                    }
                }
                is Declaration.Enum -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
            }
            if (decl != null) {
                return decl
            }
        }
        return null
    }

    fun getEnclosingFunction(node: HasLocation): FunctionDef? {
        return getEnclosingScopeTree(node)
    }

    fun getEnclosingClosure(node: HasLocation): Closure? {
        return getEnclosingScopeTree(node)
    }

    fun getEnclosingWhileLoop(node: HasLocation): Statement.While? {
        return getEnclosingScopeTree(node)
    }

    fun getEnclosingMatchExpression(node: HasLocation): Match? {
        return getEnclosingScopeTree(node)
    }

    fun getEnclosingTraitDef(node: HasLocation): TraitDef? {
        return getEnclosingScopeTree(node)
    }

    fun getEnclosingExtensionDef(node: HasLocation): ExtensionDef? {
        return getEnclosingScopeTree(node)
    }

    fun getEnclosingImpl(node: HasLocation): ImplementationDef? {
        return getEnclosingScopeTree(node)
    }

    private inline fun <reified Scope : ScopeTree> getEnclosingScopeTree(at: HasLocation): Scope? {
        for (scopeNode in getScopeStack(at)) {
            if (scopeNode is Scope) {
                return scopeNode
            }
        }
        return null
    }

    fun resolveGlobalName(binder: Binder): QualifiedName {
        return sourceFileOf(binder).moduleName.append(binder.identifier.name)
    }

    fun extensionDefsInScope(node: HasLocation): Sequence<ExtensionDef> = sequence {
        val declarations = sourceFileOf(node).declarations
        yieldAll(extensionDefsInDeclarations(declarations, includeImports = true))
    }

    private fun extensionDefsInDeclarations(declarations: List<Declaration>, includeImports: Boolean): Sequence<ExtensionDef> = sequence {
        for (declaration in declarations) {
            if (includeImports && declaration is ImportAs) {
                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (sourceFile != null) {
                    yieldAll(
                        extensionDefsInDeclarations(
                            sourceFile.declarations,
                            // extensions are not transitively included
                            includeImports = false
                        )
                    )
                }
            }
            if (includeImports && declaration is ImportMembers) {
                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (sourceFile != null) {
                    yieldAll(
                        extensionDefsInDeclarations(
                            sourceFile.declarations,
                            // extensions are not transitively included
                            includeImports = false
                        )
                    )
                }
            }
            if (declaration !is ExtensionDef) {
                continue
            }
            yield(declaration)
        }
    }

    val implementationDefs by lazy {
        makeList {
            ctx.forEachSourceFile {
                addAll(implementationDefsInDeclarations(it.declarations))
            }
        }
    }

    private fun implementationDefsInDeclarations(declarations: List<Declaration>): Sequence<ImplementationDef> = sequence {
        for (declaration in declarations) {
            if (declaration is ImportAs) {
                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (sourceFile != null) {
                    yieldAll(
                        implementationDefsInDeclarations(
                            sourceFile.declarations
                        )
                    )
                }
            }
            if (declaration !is ImplementationDef) {
                continue
            }
            yield(declaration)
        }
    }

    fun findTraitInSourceFile(name: Identifier, sourceFile: SourceFile): TraitDef? {
        for (declaration in sourceFile.declarations) {
            if (declaration is TraitDef) {
                if (declaration.name.identifier.name == name.name) {
                    return declaration
                }
            }
        }
        return null
    }

    fun onParseMatchArm(arm: Match.Arm) {
        addScopeNode(arm.value.location.file, arm)
    }

    fun onParseMatchExpression(expr: Match) {
        addScopeNode(expr.value.location.file, expr)
    }

    fun getSourceFile(file: SourcePath): SourceFile {
        return requireNotNull(sourceFiles[file])
    }

    fun resolveModuleAlias(name: Identifier): SourceFile? {
        for (scopeTree in getScopeStack(name)) {
            val found = when (scopeTree) {
                is SourceFile ->
                    scopeTree.declarations.filterIsInstance<ImportAs>().find {
                        it.asName.identifier.name == name.name
                    }?.let { importAs ->
                        ctx.resolveSourceFile(importAs.modulePath)
                    }
                else -> null
            }

            if (found != null) {
                return found
            }
        }
        return null
    }
}
