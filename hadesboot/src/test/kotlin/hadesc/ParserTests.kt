package hadesc

import hadesc.context.Context
import hadesc.location.SourcePath
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.text.Text
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.io.path.Path
import kotlin.test.assertEquals

class ParserTests {
    private val ctx = mockk<Context> {
        every { resolver } returns mockk {
            every { onParseBlock(any()) } returns unit
            every { onParseSourceFile(any()) } returns unit
            every { onParseDeclaration(any()) } returns unit
            every { onParseClosure(any()) } returns unit
            every { onParseMatchArm(any()) } returns unit
            every { onParseMatchExpression(any()) } returns unit
            every { onParseScopeNode(any()) } returns unit
        }
        every { makeName(any()) } answers { Name(invocation.args[0] as String) }
    }

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