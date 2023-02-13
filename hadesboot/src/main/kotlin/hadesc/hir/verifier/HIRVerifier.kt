package hadesc.hir.verifier

import hadesc.hir.HIRBlockVisitor
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRModule
import hadesc.location.SourceLocation
import hadesc.logging.logger
import kotlin.system.exitProcess

class HIRVerifier(private val module: HIRModule) : HIRBlockVisitor {
    private val log = logger(HIRVerifier::class.java)
    private val fnVerifier = HIRFunctionVerifier(module)
    fun verify() {
        log.info("HIRVerifier running on module")
        for (definition in module.definitions) {
            if (definition is HIRDefinition.Function) {
                fnVerifier.verifyFunctionDef(definition)
            }
        }
    }
}

class HIRFunctionVerifier(private val module: HIRModule) : HIRBlockVisitor {
    fun verifyFunctionDef(fn: HIRDefinition.Function) {
        val defs = module.findDefinitions(fn.name)
        if (defs.size > 1) {
            error(fn.location, "Duplicate definition of function ${fn.name.mangle()};")
        }
    }

    fun error(location: SourceLocation, message: String) {
        System.err.println("${location.file}:${location.start.line}: $message")
        exitProcess(1)
    }
}
