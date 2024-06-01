package hadesboot.middle

import kotlin.test.Test

class HIRExecutionTest {
    @Test
    fun `should execute hello world`() {
        val module = buildModule("test") {
            addFn("main") {
                returns(i32)
                addEntry {
                    emitReturn(i32(42))
                }
            }
        }
        val pretty = module.prettyPrint()
        println(pretty)
    }
}