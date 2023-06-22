package mir

import mir.MIRValue.I32
import mir.backend.emitC
import java.nio.file.Path
import kotlin.io.path.createDirectories
import kotlin.io.path.exists
import kotlin.test.Test
import kotlin.test.assertEquals

class MIRExecutionTest {
    @Test
    fun `return zero`() {
        val main = buildModule("main.mir") {
            addFunction("main", MIRType.I32) {
                addBlock("entry") {
                    emitReturn(I32(0))
                }
            }
        }

        assertEquals(0, main.execute())
    }


    @Test
    fun `return one`() {
        assertEquals(1, buildModule("main.mir") {
            addFunction("main", MIRType.I32) {
                addBlock("entry") {
                    emitReturn(I32(1))
                }
            }
        }.execute())
    }

    @Test
    fun `2 + 2`() {
        assertEquals(4, buildModule("main.mir") {
            addFunction("main", MIRType.I32) {
                addBlock("entry") {
                    emitIAdd("result", I32(2), I32(2))
                    emitReturn(localRef("result"))
                }
            }
        }.execute())
    }

    @Test
    fun `return static`() = assertEquals(5, buildModule("main.mir"){
        addStatic("foo", I32(5))

        addFunction("main", MIRType.I32) {
            addBlock("entry") {
                emitReturn(globalRef("foo"))
            }
        }
    }.execute())

    @Test
    fun `should be able to do a widening cast`() = buildModule("main.mir") {
        addFunction("main", MIRType.I32) {
            addBlock("entry") {
                emitIntWideningCast("result", MIRValue.U8(6) , MIRType.I32)
                emitReturn(localRef("result"))
            }
        }
    }.execute().let {
        assertEquals(6, it)
    }

    @Test
    fun `should call function with no params`() = buildModule("main.mir") {
        addFunction("foo", MIRType.I32) {
            addBlock("entry") {
                emitReturn(I32(5))
            }
        }

        addFunction("main", MIRType.I32) {
            addBlock("entry") {
                emitCall("result", globalRef("foo"))
                emitReturn(localRef("result"))
            }
        }

    }.execute {
        assertEquals(5, exitCode)
    }

    @Test
    fun `should call function with params`() = buildModule("main.mir") {
        addFunction("sum", MIRType.I32) {
            addParam("a", MIRType.I32)
            addParam("b", MIRType.I32)
            addBlock("entry") {
                emitIAdd("res", paramRef("a"), paramRef("b"))
                emitReturn(localRef("res"))
            }
        }

        addFunction("main", MIRType.I32) {
            addBlock("entry") {
                emitCall("result", globalRef("sum"), I32(2), I32(3))
                emitReturn(localRef("result"))
            }
        }

    }.execute {
        assertEquals(5, exitCode)
    }

    private fun MIRModule.execute(): Int {
        if (!Path.of("test_build", "mir").exists()) {
            Path.of("test_build", "mir").createDirectories()
        }
        val outputPath = Path.of("test_build", "mir", "test")
        emitC(outputPath)
        check(outputPath.exists())

        return ProcessBuilder()
            .command(outputPath.toString())
            .start()
            .waitFor()
    }

    private fun MIRModule.execute(run: ExecuteTestScope.() -> Unit) {
        val exitCode = execute()
        ExecuteTestScope(exitCode).run()
    }
}
private data class ExecuteTestScope(val exitCode: Int)