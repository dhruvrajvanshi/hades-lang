package hadesboot.middle

import kotlin.test.Test

class HIRExecutionTest {
    @Test
    fun `should execute hello world`() {
        val module = buildModule("test") {
            addFn("main") {
                returns(i32)
            }
        }
        println(module.prettyPrint())
    }
}