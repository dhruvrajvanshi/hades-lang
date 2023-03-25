package hadesc

import hadesc.location.SourcePath
import hadesc.parser.Lexer
import hadesc.text.Text
import kotlin.io.path.Path
import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTests {
    @Test
    fun `lexer offset should be correct`() {
        val text = "def foo bar ("
        val lexer = makeLexer(text)
        assertEquals(0, lexer.offset)

        lexer.nextToken()
        assertEquals(3, lexer.offset)

        lexer.nextToken()
        assertEquals(7, lexer.offset)

    }


}
private fun makeLexer(text: String) =
    Lexer(SourcePath(Path("test.hds")), Text.from(text))