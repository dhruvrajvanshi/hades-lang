package hadesc.analysis

import hadesc.ast.Declaration
import hadesc.ast.SourceFile
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

fun typecheck(sourceFiles: List<SourceFile>) {
    TypeChecker().check(sourceFiles)
}

class TypeChecker {
    fun check(program: List<SourceFile>) {
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
        is Declaration.ConstDefinition -> TODO()
        is Declaration.Enum -> TODO()
        is Declaration.Error -> TODO()
        is Declaration.ExtensionDef -> TODO()
        is Declaration.ExternConst -> TODO()
        is Declaration.ExternFunctionDef -> TODO()
        is Declaration.FunctionDef -> TODO()
        is Declaration.ImplementationDef -> TODO()
        is Declaration.ImportAs -> TODO()
        is Declaration.ImportMembers -> TODO()
        is Declaration.Struct -> TODO()
        is Declaration.TraitDef -> TODO()
        is Declaration.TypeAlias -> TODO()
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