package hadesboot.middle

import hadesboot.prettyprint.PPNode
import hadesboot.prettyprint.PPNode.*
import hadesboot.prettyprint.prettyPrint

fun Module.prettyPrint(): String = toPP().prettyPrint()

private fun Module.toPP(): PPNode = Nodes(
    items.map { it.toPP() + Text("\n") }
)

private fun Item.toPP(): PPNode = when (this) {
    is Fn -> Nodes(
        Text("fn $name"),
        parameters
            .concatWithSeparator(",") { it.toPP() }
            .withSurroundingGroup("(", ")"),
        SpaceOrLine,
        Text("->"),
        SpaceOrLine,
        returnType.toPP(),
        SpaceOrLine,
        Text("{\n"),
        (listOf(entry) + blocks).toPP(),
        Text("\n}")
    )
    is ExternFn -> Text("#todo: Extern function")
}

private fun List<Block>.toPP(): PPNode {
    return concatWithSeparator("\n") { it.toPP() }
}
private fun Block.toPP(): PPNode {
    return Group(
        Text("$label:\n"),
        Indent(
            Text("  ") + (instructions.map { it.toPP() } + listOf(terminator.toPP()))
                .concatWithSeparator("\n\n") { it }
        ),
    )

}

private fun Instruction.toPP(): PPNode = Text("#TODO: Instruction")
private fun Terminator.toPP(): PPNode = when (this) {
    is Terminator.Return -> Text("return") + SpaceOrLine + value.toPP()
}

private fun Value.toPP(): PPNode = when (this) {
    is Constant.Int -> Text(value.toString())
    is Constant.Tuple -> members.separatedSurroundedGroup("{", "}", ",") { it.toPP() }
}

private fun Parameter.toPP(): PPNode = Text("$name: ") + type.toPP()

private fun Type.toPP(): PPNode = when(this) {
    is Type.Int -> Text(str())
    is Type.Tuple -> Text("#TODO: Tuple")
}

private fun Type.Int.str(): String =
    (if (sign == Type.Sign.Signed) "i" else "u") + (
    when (width) {
        Type.Width.W32 -> "32"
        Type.Width.Size -> "size"
    })


fun List<PPNode>.toNodes(): Nodes {
    return Nodes(this.toList())
}
fun <T> List<T>.concatWithSeparator(separatorText: String, f: (T) -> PPNode): PPNode {
    val withSeparators = mapIndexed { index, it ->
        val separator: PPNode = if (index == lastIndex) {
            IfWrap(Text(separatorText), Text(""))
        } else {
            Text(separatorText) + SpaceOrLine
        }
        return f(it) + separator
    }
    return withSeparators.toNodes()
}
fun <T> List<T>.separatedSurroundedGroup(
    start: String,
    end: String,
    separator: String,
    f: (T) -> PPNode
): Group {
    return Group(
        Text(start),
        concatWithSeparator(separator, f),
        Text(end)
    )
}

fun PPNode.withSurroundingGroup(start: String, end: String): Group {
    return Group(
        Text(start),
        LineIfWrapping,
        this,
        LineIfWrapping,
        Text(end)
    )
}