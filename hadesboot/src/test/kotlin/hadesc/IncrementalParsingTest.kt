package hadesc

import hadesc.location.SourcePath
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.text.Text
import hadesc.utils.mockParsingContext
import kotlin.io.path.Path
import kotlin.test.Test


class IncrementalParsingTest {
    @Test
    fun `should return the same declaration instance as before on changes`() {
        val originalText = """
            def x(): Void {}
            def foo(): Void {  }
            def bar(): Void {}
        """.trimIndent()

        val changeText = "{  }"
        val newText = "{\n  val x = 10\n}"
        val changeStartIndex = originalText.indexOf(changeText)
        val changeStopIndex = changeStartIndex + changeText.length
        val edit = Parser.TextEdit(
            Parser.Range(changeStartIndex, changeStopIndex),
            newText
        )
        val changedText = originalText.replace(changeText, newText)
        val p1 = makeParser(originalText)
        val originalSourceFile = p1.parseSourceFile()

        val p2 = Parser.incremental(Text.from(changedText), edit, p1)

        val changedSourceFile = p2.parseSourceFile()
        TODO()
    }
}

private fun makeParser(s: String): Parser =
    Parser(mockParsingContext, QualifiedName(emptyList()), SourcePath(Path("test.hds")), Text.from(s))