package hadesc.resolver

import hadesc.Name
import hadesc.ast.*
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import llvm.makeList

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


    fun resolveTraitDef(name: Identifier): Declaration.TraitDef? {
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
        is ScopeTree.SealedTypeDef -> {
            val param = scopeNode.declaration.typeParams?.findLast {
                it.binder.identifier.name == ident.name
            }
            if (param != null) {
                TypeBinding.TypeParam(param.binder)
            } else {
                null
            }
        }
        is ScopeTree.TypeAlias -> {
            val param = scopeNode.declaration.typeParams?.find {
                it.binder.identifier.name == ident.name
            }
            if (param != null) TypeBinding.TypeParam(param.binder) else null
        }
        is ScopeTree.ExtensionDef -> {
            val param = scopeNode.declaration.typeParams?.find {
                it.binder.identifier.name == ident.name
            }
            if (param != null) TypeBinding.TypeParam(param.binder) else null
        }
        is ScopeTree.TraitDef -> {
            if (scopeNode.declaration.name.identifier.name == ident.name) {
                TypeBinding.Trait(scopeNode.declaration)
            }

            val associatedType = scopeNode.declaration.members.filterIsInstance<Declaration.TraitMember.AssociatedType>()
                .find { it.binder.name == ident.name }
            if (associatedType != null) {
                TypeBinding.AssociatedType(associatedType.binder)
            } else {
                scopeNode.declaration.params.find {
                    it.binder.identifier.name == ident.name
                }?.let { TypeBinding.TypeParam(it.binder) }
            }

        }
        is ScopeTree.ImplementationDef -> {
            val aliasDef = scopeNode.declaration.body.filterIsInstance<Declaration.TypeAlias>().find {
                it.name.name == ident.name
            }
            if (aliasDef != null) {
                TypeBinding.TypeAlias(aliasDef)
            } else {
                scopeNode.declaration.typeParams?.find {
                    it.binder.identifier.name == ident.name
                }?.let { TypeBinding.TypeParam(it.binder) }
            }

        }
        is ScopeTree.Closure -> null
        is ScopeTree.WhenArm -> null
        is ScopeTree.WhenExpression -> null
        is ScopeTree.MatchExpression -> null
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

    fun findTypeInSourceFile(ident: Identifier, sourceFile: SourceFile): TypeBinding? {
        for (declaration in sourceFile.declarations) {
            val binding = if (declaration is Declaration.Struct && declaration.binder.identifier.name == ident.name) {
                TypeBinding.Struct(declaration)
            } else if (declaration is Declaration.TypeAlias && declaration.name.identifier.name == ident.name) {
                TypeBinding.TypeAlias(declaration)
            } else if (declaration is Declaration.TraitDef && declaration.name.identifier.name == ident.name) {
                TypeBinding.Trait(declaration)
            } else if (declaration is Declaration.SealedType && declaration.name.identifier.name == ident.name) {
                TypeBinding.SealedType(declaration)
            } else if (declaration is Declaration.ImportMembers) {
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
        is ScopeTree.FunctionDef -> findInFunctionDef(ident, scope)
        is ScopeTree.SourceFile -> findInSourceFile(ident.name, scope.sourceFile)
        is ScopeTree.Block -> findInBlock(ident, scope)
        is ScopeTree.Struct -> null
        is ScopeTree.TypeAlias -> null
        is ScopeTree.ExtensionDef -> null
        is ScopeTree.TraitDef -> null
        is ScopeTree.ImplementationDef -> null
        is ScopeTree.Closure -> findInClosure(ident, scope)
        is ScopeTree.SealedTypeDef -> null
        is ScopeTree.WhenArm -> findInWhenArm(ident, scope)
        is ScopeTree.WhenExpression -> null
        is ScopeTree.MatchExpression -> null
    }

    private fun findInWhenArm(ident: Identifier, scope: ScopeTree.WhenArm): Binding? {
        return when (scope.whenArm) {
            is Expression.WhenArm.Is -> {
                if (scope.whenArm.name?.identifier?.name == ident.name) {
                    Binding.WhenArm(scope.whenArm.name, scope.whenArm)
                } else {
                    null
                }
            }
            is Expression.WhenArm.Else -> null
        }
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

    fun findInSourceFile(name: Name, sourceFile: SourceFile): Binding? {
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
                is Declaration.ExternConst -> {
                    if (declaration.name.identifier.name == name) {
                        Binding.ExternConst(declaration)
                    } else {
                        null
                    }
                }
                is Declaration.TypeAlias -> null
                is Declaration.ExtensionDef -> null
                is Declaration.TraitDef -> null
                is Declaration.ImplementationDef -> null
                is Declaration.ImportMembers -> {
                    val importedSourceFile = ctx.resolveSourceFile(declaration.modulePath)
                    if (importedSourceFile != null && declaration.names.any { it.name == name }) {
                        findInSourceFile(name, importedSourceFile)
                    } else {
                        null
                    }
                }
                is Declaration.SealedType -> if (name == declaration.name.identifier.name) {
                    Binding.SealedType(declaration)
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

    private fun findInClosure(ident: Identifier, scope: ScopeTree.Closure): Binding? {
        var index = -1
        for (param in scope.closure.params) {
            index++
            if (param.binder.identifier.name == ident.name) {
                return Binding.ClosureParam(index, scope.closure)
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
            .getOrDefault(node.location.file, emptyList())
            .sortedByDescending { it.location }
            .filter { it.location contains node }

        return ScopeStack(scopes)
    }

    fun qualifiedStructName(declaration: Declaration.Struct): QualifiedName {
        return sourceFileOf(declaration).moduleName.append(declaration.binder.identifier.name)
    }

    fun qualifiedName(name: Binder): QualifiedName {
        return sourceFileOf(name).moduleName.append(name.identifier.name)
    }

    fun onParseClosure(closure: Expression.Closure) {
        addScopeNode(closure.location.file, ScopeTree.Closure(closure))
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
            is Declaration.TypeAlias -> {
                addScopeNode(declaration.location.file, ScopeTree.TypeAlias(declaration))
            }
            is Declaration.ExtensionDef -> {
                addScopeNode(declaration.location.file, ScopeTree.ExtensionDef(declaration))
            }
            is Declaration.TraitDef -> {
                addScopeNode(declaration.location.file, ScopeTree.TraitDef(declaration))
            }
            is Declaration.ImplementationDef -> {
                addScopeNode(declaration.location.file, ScopeTree.ImplementationDef(declaration))
            }
            is Declaration.SealedType -> {
                addScopeNode(declaration.location.file, ScopeTree.SealedTypeDef(declaration))
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
                is ScopeTree.Closure -> null
                is ScopeTree.SourceFile -> {
                    var binding: Binding? = null
                    for (declaration in scope.sourceFile.declarations) {
                        binding = when (declaration) {
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
                is Declaration.TypeAlias -> decl.name.identifier.name == declName
                is Declaration.ExtensionDef -> false
                is Declaration.TraitDef -> decl.name.identifier.name == declName
                is Declaration.ImplementationDef -> false
                is Declaration.ImportMembers -> false
                is Declaration.SealedType -> decl.name.identifier.name == declName
                is Declaration.ExternConst -> decl.name.identifier.name == declName
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
                is Declaration.ExternConst -> if (declaration.name.identifier.name == name.name) {
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
                is Declaration.TypeAlias -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else {
                    null
                }
                is Declaration.ExtensionDef -> null
                is Declaration.TraitDef -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else null
                is Declaration.ImplementationDef -> null
                is Declaration.ImportMembers -> {
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
                is Declaration.SealedType -> if (declaration.name.identifier.name == name.name) {
                    declaration
                } else null
            }
            if (decl != null) {
                return decl
            }
        }
        return null

    }

    fun getEnclosingFunction(node: HasLocation): Declaration.FunctionDef? {
        return getEnclosingScopeTree<ScopeTree.FunctionDef>(node)?.declaration
    }

    fun getEnclosingTraitDef(node: HasLocation): Declaration.TraitDef? {
        return getEnclosingScopeTree<ScopeTree.TraitDef>(node)?.declaration
    }

    fun getEnclosingExtensionDef(node: HasLocation): Declaration.ExtensionDef? {
        return getEnclosingScopeTree<ScopeTree.ExtensionDef>(node)?.declaration
    }

    fun getEnclosingWhenExpression(node: HasLocation): Expression.When? {
        return getEnclosingScopeTree<ScopeTree.WhenExpression>(node)?.expression
    }

    fun getEnclosingImpl(node: HasLocation): Declaration.ImplementationDef? {
        return getEnclosingScopeTree<ScopeTree.ImplementationDef>(node)?.declaration
    }

    fun getEnclosingMatchExpression(node: HasLocation): Expression.Match? {
        return getEnclosingScopeTree<ScopeTree.MatchExpression>(node)?.expression
    }

    private inline fun <reified Scope: ScopeTree> getEnclosingScopeTree(at: HasLocation): Scope? {
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

    fun extensionDefsInScope(node: HasLocation): Sequence<Declaration.ExtensionDef> = sequence {
        val declarations = sourceFileOf(node).declarations
        yieldAll(extensionDefsInDeclarations(declarations, includeImports = true))
    }

    private fun extensionDefsInDeclarations(declarations: List<Declaration>, includeImports: Boolean): Sequence<Declaration.ExtensionDef> = sequence {
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
            if (includeImports && declaration is Declaration.ImportMembers) {
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

    private fun implementationDefsInDeclarations(declarations: List<Declaration>): Sequence<Declaration.ImplementationDef> = sequence {
        for (declaration in declarations) {
            if (declaration is Declaration.ImportAs) {
                val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (sourceFile != null) {
                    yieldAll(implementationDefsInDeclarations(
                            sourceFile.declarations
                    ))
                }
            }
            if (declaration !is Declaration.ImplementationDef) {
                continue
            }
            yield(declaration)
        }
    }

    fun findTraitInSourceFile(name: Identifier, sourceFile: SourceFile): Declaration.TraitDef? {
        for (declaration in sourceFile.declarations) {
            if (declaration is Declaration.TraitDef) {
                if (declaration.name.identifier.name == name.name) {
                    return declaration
                }
            }
        }
        return null
    }

    fun onParseWhenArm(arm: Expression.WhenArm) {
        addScopeNode(arm.value.location.file, ScopeTree.WhenArm(arm))
    }

    fun onParseWhenExpression(expression: Expression.When) {
        addScopeNode(expression.value.location.file, ScopeTree.WhenExpression(expression))
    }

    fun getSourceFile(file: SourcePath): SourceFile {
        return requireNotNull(sourceFiles[file])
    }

    fun resolveModuleAlias(name: Identifier): SourceFile? {
        for (scopeTree in getScopeStack(name)) {
            val found = when (scopeTree) {
                is ScopeTree.SourceFile ->
                    scopeTree.sourceFile.declarations.filterIsInstance<Declaration.ImportAs>().find {
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
