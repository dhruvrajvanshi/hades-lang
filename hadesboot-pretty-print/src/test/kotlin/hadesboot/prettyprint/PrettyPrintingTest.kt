package hadesboot.prettyprint

import hadesboot.prettyprint.Node.*
import kotlin.test.Test
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
        val node =

        expect("[1, 2, 3]") {
            jsonArray123.prettyPrint(PrettyPrintConfig(
                lineWidth = 20,
                indent = "  "
            ))
        }
    }

    @Test
    fun `should wrap when needed`() {
        val node =

        expect(
            """
            [
              1,
              2,
              3,
            ]
            """.trimIndent()
        ) {
            jsonArray123.prettyPrint(PrettyPrintConfig(
                lineWidth = 5,
                indent = "  "
            ))
        }

    }

}

val jsonArray123 = Group(
    id = 0,
    Text("["),
    Line,
    Indent(
        Text("1"),
        Text(","),
        SpaceOrLine,
        Text("2"),
        Text(","),
        SpaceOrLine,
        Text("3"),
        IfWrap(0, Text(","), Text("")),
    ),
    Line,
    Text("]")
)