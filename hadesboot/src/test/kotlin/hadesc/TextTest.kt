package hadesc

import hadesc.text.Text
import kotlin.test.Test
import kotlin.test.assertEquals

class TextTest {
    @Test
    fun `offsetOf for string with one line`() {
        val str = Text.from("foo")
        assertEquals(0, str.offsetOf(1, 1))
        assertEquals(1, str.offsetOf(1, 2))
    }

    @Test
    fun `offsetOf for string with multiple lines`() {
        val str = Text.from("foo\nbar\nbax")
        assertEquals(4, str.offsetOf(2, 1))
        assertEquals(5, str.offsetOf(2, 2))
    }
}
