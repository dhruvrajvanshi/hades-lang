package hadesboot.middle

import kotlin.test.Test

class HIRExecutionTest {
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
        println(pretty)
    }
}