package hadesc.analysis

import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.Declaration
import hadesc.ast.SourceFile
import hadesc.ast.TypeAnnotation
import hadesc.diagnostics.DiagnosticReporter
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.resolver.Resolver
import hadesc.resolver.TypeBinding
import hadesc.types.Type

fun typecheck(sourceFiles: List<SourceFile>, diagnosticReporter: DiagnosticReporter, resolver: Resolver<*>) {
    TypeChecker(diagnosticReporter, sourceFiles, resolver).check()
}

class TypeChecker(
    private val diagnostic: DiagnosticReporter,
    private val program: List<SourceFile>,
    private val resolver: Resolver<*>,
) {
    fun check() {
        for (file in program) {
            visitSourceFile(file)
        }
    }

    private fun visitSourceFile(file: SourceFile) {
        for (decl in file.declarations) {
            visitDeclaration(decl)
        }
    }

    private fun visitDeclaration(decl: Declaration): Unit = when (decl) {
        is Declaration.Error -> {}
        is Declaration.ExternConst -> visitExternConst(decl)
        is Declaration.ExternFunctionDef -> visitExternFunctionDef(decl)
        is Declaration.Enum,
        is Declaration.ExtensionDef,
        is Declaration.FunctionDef,
        is Declaration.ImplementationDef,
        is Declaration.ImportAs,
        is Declaration.ImportMembers,
        is Declaration.Struct,
        is Declaration.TraitDef,
        is Declaration.TypeAlias,
        is Declaration.ConstDefinition -> todo(decl)
    }

    private fun visitExternFunctionDef(decl: Declaration.ExternFunctionDef) {
        checkTopLevelExpressionBinding(decl.binder)
        val paramTypes = decl.paramTypes.map { it.lower() }
        val returnType = decl.returnType.lower()

        for ((paramType, annotation) in paramTypes.zip(decl.paramTypes)) {
            if (!paramType.isExternSafe()) {
                diagnostic.report(annotation.location, "This type is not allowed in extern functions")
            }
        }
        if (!returnType.isExternSafe()) {
            diagnostic.report(decl.returnType.location, "This type is not allowed in extern functions")
        }
    }

    /**
     * Types that can be passed to or returned from extern functions
     */
    private fun Type.isExternSafe(): Boolean {
        return when(this) {
            is Type.Constructor,
            is Type.Integral,
            is Type.UntaggedUnion,
            is Type.Size,
            Type.Void,
            Type.Bool,
            is Type.FloatingPoint -> true
            is Type.Ptr -> to.isExternSafe()
            is Type.Application -> false
            is Type.Closure -> false
            is Type.FunctionPtr -> from.all { it.isExternSafe() } && to.isExternSafe()
            is Type.GenericInstance -> false
            is Type.Param -> false
            is Type.TypeFunction -> false
            is Type.AssociatedTypeRef -> false
            is Type.Ref -> false
            is Type.Select -> false
            is Type.Array -> itemType.isExternSafe()
            is Type.Error -> true
        }
    }

    private fun visitExternConst(decl: Declaration.ExternConst) {
        checkTopLevelExpressionBinding(decl.name)
        decl.type.lower()
    }

    private val loweredTypes = nodeMapOf<TypeAnnotation, Type>()
    private fun TypeAnnotation.lower(): Type = loweredTypes.getOrPut(this) {
        when(this) {
            is TypeAnnotation.Application -> todo(this)
            is TypeAnnotation.Array -> todo(this)
            is TypeAnnotation.Closure -> todo(this)
            is TypeAnnotation.Error -> todo(this)
            is TypeAnnotation.FunctionPtr -> todo(this)
            is TypeAnnotation.MutPtr -> Type.Ptr(to.lower(), isMutable = true)
            is TypeAnnotation.Ptr -> Type.Ptr(to.lower(), isMutable = false)
            is TypeAnnotation.Qualified -> todo(this)
            is TypeAnnotation.Select -> todo(this)
            is TypeAnnotation.Union -> todo(this)
            is TypeAnnotation.Var -> {
                when (val binding = resolver.resolveTypeVariable(name)) {
                    is TypeBinding.Builtin -> binding.type
                    is TypeBinding.AssociatedType -> todo(this)
                    is TypeBinding.Enum -> todo(this)
                    is TypeBinding.Struct -> todo(this)
                    is TypeBinding.Trait -> todo(this)
                    is TypeBinding.TypeAlias -> todo(this)
                    is TypeBinding.TypeParam -> todo(this)
                    null -> {
                        reportAndMakeErrorType(location,  "Can not find `${name.name.text}` in this scope")
                    }
                }
            }
        }
    }

    private fun reportAndMakeErrorType(location: SourceLocation, message: String): Type.Error {
        diagnostic.report(location, message)
        return Type.Error(location, message)
    }

    private fun todo(decl: Declaration) {
        diagnostic.report(decl.startLoc, "The new typechecker doesn't support this declaration type yet.")
    }

    private fun todo(type: TypeAnnotation): Type {
        val message = "The new typechecker doesn't support this node type yet."
        diagnostic.report(type.location, message)
        return Type.Error(type.location, message)
    }

    private val topLevelExpressionBindingsByFile = mutableMapOf<SourcePath, MutableMap<Name, Binder>>()
    private fun checkTopLevelExpressionBinding(binder: Binder) {
        val topLevelExpressionBindings = topLevelExpressionBindingsByFile.getOrPut(binder.location.file) { mutableMapOf() }
        if (binder.name in topLevelExpressionBindings) {
            diagnostic.report(binder.location, "Duplicate top-level binding ${binder.name}")
        } else {
            topLevelExpressionBindings[binder.name] = binder
        }
    }
}

private fun <T: HasLocation, V> nodeMapOf() = NodeMap<T, V>()
private class NodeMap<T : HasLocation, V> {
    private val map = mutableMapOf<SourceLocation, V>()

    fun getOrPut(key: T, compute: () -> V): V {
        val existing = map[key.location]
        if (existing != null) {
            return existing
        }
        val value = compute()
        map[key.location] = value
        return value
    }

    operator fun get(key: T): V? {
        return map[key.location]
    }

    operator fun set(key: T, value: V) {
        map[key.location] = value
    }
}