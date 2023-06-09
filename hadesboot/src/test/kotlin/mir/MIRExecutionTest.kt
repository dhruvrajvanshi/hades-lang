package mir

import mir.backend.emitC
import kotlin.test.Test

class MIRExecutionTest {
    @Test
    fun `return zero`() {
        val main = buildObject {
            addValue("main", buildFunction(returnType = MIRType.I32) {
                addBlock("entry") {
                    emitReturn(MIRValue.I32(0))
                }
            })
        }
        main.emitC()
    }
}