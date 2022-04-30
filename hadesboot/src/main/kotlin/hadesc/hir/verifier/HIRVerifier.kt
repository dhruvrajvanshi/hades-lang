package hadesc.hir.verifier

import hadesc.analysis.TypeAnalyzer
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRModule
import hadesc.hir.HIRBlockVisitor
import hadesc.hir.HIRStatement
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.types.Type
import kotlin.system.exitProcess

class HIRVerifier(private val module: HIRModule): HIRBlockVisitor {
    private val fnVerifier = HIRFunctionVerifier(module)
    fun verify() {
        logger().info("HIRVerifier running on module")
        for (definition in module.definitions) {
            if (definition is HIRDefinition.Function) {
                fnVerifier.verifyFunctionDef(definition)
            }
        }
    }
}

class HIRFunctionVerifier(private val module: HIRModule): HIRBlockVisitor {
    private val typeAnalyzer = TypeAnalyzer()
    fun verifyFunctionDef(fn: HIRDefinition.Function) {
        val defs = module.findDefinitions(fn.name)
        if (defs.size > 1) {
            error(fn.location, "Duplicate definition of function ${fn.name.mangle()};")
        }

        for (block in fn.basicBlocks) {
            visitBlock(block)
        }
    }

    override fun visitStore(statement: HIRStatement.Store) {
        super.visitStore(statement)
        val ptrType = statement.ptr.type
        if (ptrType !is Type.Ptr) {
            error(statement.location, "Store to a non pointer type: ${ptrType.prettyPrint()}")
        }

        if (!typeAnalyzer.isTypeAssignableTo(source = statement.value.type, destination = ptrType.to)) {
            error(statement.location, "${statement.value.type.prettyPrint()} is not assignable to ${ptrType.to.prettyPrint()}") {
                statement.prettyPrint()
            }
        }
    }

    fun error(location: SourceLocation, message: String, lazyMessage: () -> String = { "" }): Nothing {
        System.err.println("${location.file}:${location.start.line}: $message\n${lazyMessage()}")
        exitProcess(1)
    }
}