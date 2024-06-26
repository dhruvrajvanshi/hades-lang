package hadesboot.prettyprint

import hadesboot.prettyprint.PPNode.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.expect

class PrettyPrintingTest {

    @Test
    fun `should print a simple node`() {
        expect("Hello, World!") {
            Text("Hello, World!").prettyPrint()
        }
    }

    @Test
    fun `should print a non wrapping json array`() {

        expect("[1, 2, 3]") {
            listOf(1, 2, 3).toNode().prettyPrint(
                lineWidth = 20,
                indent = "  "
            )
        }
    }

    @Test
    fun `should wrap when needed`() {

        expect(
            """
            [
              1,
              2,
              3,
            ]
            """.trimIndent()
        ) {
            listOf(1, 2, 3).toNode().prettyPrint(
                lineWidth = 5,
                indent = "  "
            )
        }

    }

    @Test
    fun `should wrap nested groups independently`() {
        val node = listOf(
            1,
            2,
            listOf(3, 4),
            5,
            6
        ).toNode()
        val actual = node.prettyPrint(
            lineWidth = 10,
            indent = "  "
        )
        val expected = """
            [
              1,
              2,
              [3, 4],
              5,
              6,
            ]
        """.trimIndent()
        assertEquals(expected, actual)
    }

}

private fun Any?.toNode(): PPNode = when (this) {
    is String -> Text(this)
    is Int -> Text(this.toString())
    is Float -> Text(this.toString())
    is List<*> -> {
        Group(
            Text("["),
            LineIfWrapping,
            Indent(
                this.mapIndexed { index, it ->
                    val comma =
                        if (index == lastIndex) {
                            IfWrap(Text(","), Text(""))
                        } else {
                            Text(",") + SpaceOrLine
                        }
                    it.toNode() + comma
                }
            ),
            LineIfWrapping,
            Text("]")
        )
    }
    null -> Text("null")
    else -> throw IllegalArgumentException("Unsupported type: $this")
}
