package hadesc

import hadesc.text.Text
import kotlin.test.Test
import kotlin.test.assertEquals

class TextTest {
    @Test
    fun `offsetOf for string with one line`() {
        val str = Text.from("foo")
        assertEquals(str.offsetOf(1, 1), 0)
        assertEquals(str.offsetOf(1, 2), 1)
    }
}
