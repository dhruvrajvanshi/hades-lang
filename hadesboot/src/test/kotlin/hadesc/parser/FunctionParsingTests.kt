package hadesc.parser

import hadesc.text.Text
import org.junit.jupiter.api.Test
import java.nio.file.Path

class FunctionParsingTests {
    @Test
    fun `should parse empty function`() {
        val parser = Parser(Path.of(""), Text.from("""
            pub fn main() -> () {}
        """.trimIndent()));

        val item = parser.parseItem()
    }
}