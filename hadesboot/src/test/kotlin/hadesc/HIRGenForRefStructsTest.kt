package hadesc

import hadesc.context.BuildTarget
import hadesc.context.Context
import hadesc.context.FileTextProvider
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRModule
import hadesc.hir.HIRModuleVisitor
import hadesc.hirgen.HIRGen
import hadesc.qualifiedname.QualifiedName
import hadesc.text.Text
import hadesc.types.Type
import org.junit.jupiter.api.Test
import java.nio.file.Path
import kotlin.test.assertEquals

class HIRGenForRefStructsTest {
    @Test
    fun `should lower ref struct return type`() = withTestCtx(
        """
        struct BoxedU32 ref {
            val value: u32
        }
        
        def returns_boxed_u32(): BoxedU32 {
            return BoxedU32(1)
        }       
        """.trimIndent()
    ) {
        val fn = findGlobalFunction("returns_boxed_u32")

        val returnType = fn.returnType

        assertEquals(actual = returnType, expected = Type.Ref(tycon("BoxedU32")))
    }

    @Test
    fun `should lower ref struct param type`() = withTestCtx(
        """
        struct BoxedU32 ref {
            val value: u32
        }
        def takes_boxed_u32(param: BoxedU32): Void {
        }
        """.trimIndent()
    ) {
        val fn = findGlobalFunction("takes_boxed_u32")

        val ty = fn.params[0].type

        assertEquals(actual = ty, expected = Type.Ref(tycon("BoxedU32")))
    }

    @Test
    fun `should lower fields with ref structs`() = withTestCtx(
        """
        struct Boxed[T] ref {
            val value: T
         }
        struct ContainsBox {
            val value: Boxed[u32]
        }
        """.trimIndent()
    ) {
        assertRefStructsAreLoweredToRefType(hir, "Boxed")
    }

    @Test
    fun `should lower enums with ref structs`() = withTestCtx(
        """
        struct Boxed[T] ref {
          val value: T
        }
        enum ContainsBox[T] {
          Foo(Boxed[T])
          Bar(Boxed[u32])
        }
        """.trimIndent()
    ) {
        assertRefStructsAreLoweredToRefType(hir, "Boxed")
    }

    @Test
    fun `should lower struct field assignments to ref types`() = withTestCtx(
        """
        struct Boxed[T] ref {
          val value: T
        }
        
        struct ContainsBox[T] {
            val box: Boxed[T]
        }
        
        def main(): Void {
            val boxed_bool = Boxed(true)
            val mut wrapper = ContainsBox(boxed_bool)
            
            wrapper.box = Boxed(false)
            
            val wrapper_box = wrapper.box
            wrapper_box.value = true
        }
        """.trimIndent()
    ) {
        assertRefStructsAreLoweredToRefType(hir, "Boxed")
    }

    @Test
    fun `lowering of types in match expressions`() = withTestCtx(
        """
        struct Boxed[T] ref {
          val value: T
        }
        enum ContainsBox[T] {
          Foo(Boxed[T])
        }
        
        def main(): Void {
            val tr = match ContainsBox.Foo(Boxed(true)) {
                Foo(val box) -> box.value
            }
        }
        """.trimIndent()
    ) {
        assertRefStructsAreLoweredToRefType(hir, "Boxed")
    }
}

fun TestBuilder.assertRefStructsAreLoweredToRefType(hir: HIRModule, name: String) {
    hir.visit(object : HIRModuleVisitor {
        override fun visitType(type: Type): Unit = when (type) {
            is Type.Constructor -> assert(type.name.mangle() != name)
            is Type.Ref -> {
                when (val inner = type.inner) {
                    is Type.Constructor -> unit
                    is Type.Application -> {
                        // This type is wrapped by ref, we don't need to our callee
                        inner.args.forEach { visitType(it) }
                    }
                    else -> super.visitType(type.inner)
                }
            }
            else -> super.visitType(type)
        }
    })

    assertEquals(ctx.diagnosticReporter.errors, emptyList())
}
fun HIRModule.visit(visitor: HIRModuleVisitor) {
    visitor.visitModule(this)
}

interface TestBuilder {
    val ctx: Context
    val hir: HIRModule

    fun findGlobalFunction(name: String): HIRDefinition.Function {
        val qn = QualifiedName(name.split(".").map { ctx.makeName(it) })
        return hir.findGlobalFunction(qn)
    }

    fun tycon(name: String) = Type.Constructor(QualifiedName(name.split(".").map { ctx.makeName(it) }))
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
        fileTextProvider = object : FileTextProvider {
            override fun getFileText(path: Path): Text {
                check(path == Path.of("main.hds"))
                return Text.from(source)
            }
        }
    )
    val hirGen = HIRGen(ctx)

    val sourceFile = requireNotNull(ctx.resolveSourceFile(QualifiedName()))

    val module = hirGen.lowerSourceFiles(listOf(sourceFile))

    val testCtx = object : TestBuilder {
        override val ctx: Context = ctx
        override val hir: HIRModule = module
    }
    testCtx.build()
}
