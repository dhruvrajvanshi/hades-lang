package hadesboot.middle

import hadesboot.prettyprint.PPNode
import hadesboot.prettyprint.PPNode.*
import hadesboot.prettyprint.prettyPrint

fun Module.prettyPrint(): String = toPP().prettyPrint(lineWidth = 80)

private fun Module.toPP(): PPNode = Group(
    items.map { it.toPP() + LineIfWrapping }
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
        Group(
            Text("{"),
            LineIfWrapping,
            Group((listOf(entry) + blocks).map { it.toPP() }.joinWith(LineIfWrapping + LineIfWrapping)).forceWrap(),
            LineIfWrapping,
            Text("}")
        ).forceWrap()
    )
    is ExternFn -> Text("#todo: Extern function")
}

private fun List<Block>.toPP(): PPNode {
    return Group(
        map { it.toPP() }.joinWith(LineIfWrapping + LineIfWrapping)
    ).forceWrap()
}
private fun Block.toPP(): PPNode {
    val nodes = instructions.map { it.toPP() } + listOf(terminator.toPP())
    return Group(
        Text("$label:"),
        LineIfWrapping,
        Indent(
            nodes.map {
                Group(it)
            }.joinWith(LineIfWrapping)
        ),
    ).forceWrap()
}

private fun List<PPNode>.joinWith(separator: PPNode): List<PPNode> {
    return flatMap { listOf(separator, it) }.drop(1)
}

private fun Instruction.toPP(): PPNode = Text("#TODO: Instruction")
private fun Terminator.toPP(): PPNode = when (this) {
    is Terminator.Return -> Text("return") + SpaceOrLine + Indent(value.toPP())
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
        f(it) + separator
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