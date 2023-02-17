package hadesc

import hadesc.context.BuildTarget
import hadesc.context.Context
import hadesc.context.FileTextProvider
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRModule
import hadesc.hirgen.HIRGen
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import org.junit.jupiter.api.Test
import java.nio.file.Path
import kotlin.test.assertIs

class HIRGenForRefStructsTest {
    @Test
    fun `should lower ref struct return type`() = withTestCtx("""
        struct BoxedU32 ref {
            val value: u32
        }
        
        def returns_boxed_u32(): BoxedU32 {
            return BoxedU32(1)
        }       
    """.trimIndent()) {

        val fn = findGlobalFunction("returns_boxed_u32")

        val returnType = fn.returnType
        assertIs<Type.Ptr>(returnType)

    }

    @Test
    fun `should lower ref struct param type`() = withTestCtx("""
        struct BoxedU32 ref {
            val value: u32
        }
        def takes_boxed_u32(param: BoxedU32): Void {
        }
    """.trimIndent()) {
        val fn = findGlobalFunction("takes_boxed_u32")

        val ty = fn.params[0].type

        assertIs<Type.Ptr>(ty)
    }
}
interface TestBuilder {
    val ctx: Context
    val hir: HIRModule

    fun findGlobalFunction(name: String): HIRDefinition.Function {
        val qn = QualifiedName(name.split(".").map { ctx.makeName(it) })
        return hir.findGlobalFunction(qn)
    }
}
fun withTestCtx(source: String, build: TestBuilder.() -> Unit) {
    val ctx = Context(
        BuildOptions(
            directories = emptyList(),
            runtime = Path.of("runtime.c"),
            cFlags = emptyList(),
            debugSymbols = true,
            cSources = emptyList(),
            dumpLLVMModule = false,
            libs = emptyList(),
            enableHIRVerifier = true,
            dumpHIRGen = false,
            enableLLVMVerifier = false,
            jsonDiagnostics = false
        ),
        BuildTarget.Executable(Path.of("main.hds"), Path.of("")),
        fileTextProvider = object: FileTextProvider {
            override fun getFileText(path: Path): String {
                check(path == Path.of("main.hds"))
                return source
            }
        }
    )
    val hirGen = HIRGen(ctx)

    val sourceFile = requireNotNull(ctx.resolveSourceFile(QualifiedName()))

    val module = hirGen.lowerSourceFiles(listOf(sourceFile))

    val testCtx = object: TestBuilder {
        override val ctx: Context = ctx
        override val hir: HIRModule = module
    }
    testCtx.build()
}
