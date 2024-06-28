package hadesc.analysis

import hadesc.ast.Declaration
import hadesc.ast.SourceFile
import hadesc.diagnostics.DiagnosticReporter
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

fun typecheck(sourceFiles: List<SourceFile>, diagnosticReporter: DiagnosticReporter) {
    TypeChecker(diagnosticReporter).check(sourceFiles)
}

class TypeChecker(
    private val diagnostic: DiagnosticReporter
) {
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
        is Declaration.Error -> {}
        is Declaration.Enum,
        is Declaration.ExtensionDef,
        is Declaration.ExternConst,
        is Declaration.ExternFunctionDef,
        is Declaration.FunctionDef,
        is Declaration.ImplementationDef,
        is Declaration.ImportAs,
        is Declaration.ImportMembers,
        is Declaration.Struct,
        is Declaration.TraitDef,
        is Declaration.TypeAlias,
        is Declaration.ConstDefinition -> todo(decl)

    }

    private fun todo(decl: Declaration) {
        diagnostic.report(decl.startLoc, "The new typechecker doesn't support this declaration type yet.")
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