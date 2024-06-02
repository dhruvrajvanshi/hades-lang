package hadesboot.middle

import kotlin.test.Test
import kotlin.test.expect

class HIRPPTest {
    @Test
    fun `should execute hello world`() {
        val module = buildModule("test") {
            addFn("main") {
                returnType = i32
                entry = buildBlock("entry") {
                    emitReturn(i32(42))
                }
            }
        }
        val pretty = module.prettyPrint()
        expect(pretty) {
            """
            fn main() -> i32 {
            entry:
              return i32 42
            }
            """.trimIndent()
        }
    }
}