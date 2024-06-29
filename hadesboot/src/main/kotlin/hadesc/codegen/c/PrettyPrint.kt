package hadesc.codegen.c

import hadesboot.prettyprint.PPNode
import hadesboot.prettyprint.PPNode.Text

fun CNode.toPPNode(): PPNode = when (this) {
    is CNode.Include -> Text("#include \"$path\"")
    is CNode.Raw -> Text(code)
    is CNode.PtrType -> Text(if (isConst) "const " else " ") + type.toPPNode() + Text("*")
    is CNode.FnSignature -> PPNode.Group(
        if (isExtern) Text("extern") else Text(""),
        Text(" "),
        returnType.toPPNode(),
        Text(" "),
        Text(name),
        PPNode.Group(
            Text("("),
            PPNode.Indent(
                parameters.mapIndexed { index, it ->
                    it.toPPNode() + if (index == parameters.lastIndex) Text("") else Text(", ")
                }
            ),
            Text(")"),
            Text(";")
        )
    )
    is CNode.ExternConst -> {
        PPNode.Group(
            Text("extern const "),
            type.toPPNode(),
            Text(" "),
            Text(name),
            Text(";")
        )
    }
}

fun declarationsToPPNode(declarations: List<CNode>): PPNode {
    return PPNode.Group(
        declarations.mapIndexed { idx, it ->
            it.toPPNode() + Text("\n") +
                if (idx == declarations.lastIndex) Text("") else Text("\n")
        }
    )
}