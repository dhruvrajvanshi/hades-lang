package hadesc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class UtilsTest {
    @Test
    fun clampToPowerOfTwo() {
        assertEquals(clampToPowerOfTwo(2), 2)
        assertEquals(clampToPowerOfTwo(4), 4)
        assertEquals(clampToPowerOfTwo(3), 4)
        assertEquals(clampToPowerOfTwo(5), 8)
        assertEquals(clampToPowerOfTwo(8), 8)
        assertEquals(clampToPowerOfTwo(12), 16)
        assertEquals(clampToPowerOfTwo(36), 64)
    }
}
