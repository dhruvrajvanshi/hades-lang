package hadesc

import hadesc.location.SourcePath
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.text.Text
import hadesc.utils.mockParsingContext
import org.junit.jupiter.api.Test
import kotlin.io.path.Path
import kotlin.test.assertEquals

class ParserTests {
    private val ctx = mockParsingContext
    @Test
    fun `sourceFiles have correct length`() {
        val text = """
            def foo(): Void {}
        """.trimIndent()
        val parser = makeParser(text)
        val sourceFile = parser.parseSourceFile()

        assertEquals(
            text.length,
            sourceFile.length,
        )
    }

    @Test
    fun `Source files with leading comments have correct length`() {
        val text = """
            // this is a comment
            def foo(): Void {}
        """.trimIndent()
        val parser = makeParser(text)
        val sourceFile = parser.parseSourceFile()

        assertEquals(
            text.length,
            sourceFile.length
        )
    }

    @Test
    fun `Source files with trailing comments have the correct length`() {
        val text = """
            // this is a comment
            def foo(): Void {}
            // a trailing comment
        """.trimIndent()
        val parser = makeParser(text)
        val sourceFile = parser.parseSourceFile()

        assertEquals(
            text.length + 1, // 1 for EOF character
            sourceFile.length
        )
    }

    private fun makeParser(source: String) =
        Parser(ctx, QualifiedName(emptyList()), SourcePath(Path("test.hds")), Text.from(source))
}