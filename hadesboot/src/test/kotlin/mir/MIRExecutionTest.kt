package mir

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
                    emitReturn(MIRValue.I32(0))
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
                    emitReturn(MIRValue.I32(1))
                }
            }
        }.execute())
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
}