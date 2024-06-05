package hadesboot.middle

import hadesboot.middle.lower.lowerToC
import kotlin.test.Ignore
import kotlin.test.Test
import kotlin.test.assertEquals

class CBackendTest {
    @Test
    @Ignore
    fun `return constant from main`() {
        val module = buildModule("test") {
            addFn("main") {
                returnType = i32
                entry = buildBlock("entry") {
                    emitReturn(i32(42))
                }
            }
        }

        val c = lowerToC(module)
        assertEquals(
            """
            #include <stdint.h>
            
            int main() {
            entry:
              return 42;
            }
            """.trimIndent(),
            c
        )
    }
}