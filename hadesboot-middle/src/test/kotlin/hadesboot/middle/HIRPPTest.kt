package hadesboot.middle

import kotlin.test.Test
import kotlin.test.assertEquals

class HIRPPTest {
    @Test
    fun `should pretty print case 1`() {
        val module = buildModule("test") {
            addFn("main") {
                returnType = i32
                entry = buildBlock("entry") {
                    emitReturn(i32(42))
                }
            }
        }
        assertEquals(
            """
            fn main() -> i32 {
            entry:
              return i32 42
            }
            """.trimIndent(),
            module.prettyPrint()
        )
    }

    @Test
    fun `should pretty print case 2`() {
        val module = buildModule("test") {
            addFn("main") {
                returnType = i32
                entry = buildBlock("entry") {
                    emitReturn(i32(42))
                }
                addBlock(buildBlock("block1") {
                    emitReturn(i32(43))
                })
            }
        }
        val pretty = module.prettyPrint()
        assertEquals(
            """
            fn main() -> i32 {
            entry:
              return 42
            
            block1:
              return 43
            }
            """.trimIndent(),
            pretty
        )
    }
}