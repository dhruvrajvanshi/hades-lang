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
    is CNode.StructDef -> {
        PPNode.Group(
            Text("typedef struct $name"),
            Text("{"),
            PPNode.Indent(
                fields.map {
                    it.second.toPPNode() + Text(" ") + Text(it.first) + Text(";")
                }
            ),
            Text("} $name;")
        )
    }

    is CNode.UnionDecl -> {
        PPNode.Group(
            Text("typedef union ${name.c()}"),
            Text("{"),
            PPNode.Indent(
                members.mapIndexed { idx, it ->
                    it.toPPNode() + Text(" ") + Text("_$idx") + Text(";")
                }
            ),
            Text("} ${name.c()};")
        )
    }
    is CNode.TypedefFnPtr -> PPNode.Group(
        Text("typedef "),
        returnType.toPPNode(),
        Text(" (*${name.c()})"),
        Text("("),
        PPNode.Indent(
            parameters.mapIndexed { idx, it ->
                it.toPPNode() + Text(if (idx == parameters.lastIndex) "" else ", ")
            }
        ),
        Text(");")

    )
    is CNode.FnDefinition -> PPNode.Group(
        returnType.toPPNode(),
        Text(" "),
        Text(name),
        Text("("),
        PPNode.Indent(
            parameters.mapIndexed { idx, it ->
                it.toPPNode() + Text(if (idx == parameters.lastIndex) "" else ", ")
            }
        ),
        Text(")"),
        body.toPPNode()

    )

    is CNode.ConstDef -> PPNode.Group(
        Text("const "),
        type.toPPNode(),
        Text(" "),
        Text(name),
        Text(" = "),
        initializer.toPPNode(),
        Text(";")
    )

    is CNode.Block -> PPNode.Group(
        Text("{"),
        PPNode.Indent(
            items.map {
                it.toPPNode() + PPNode.LineIfWrapping
            }
        ),
        Text("}"),
    )

    is CNode.Assign -> PPNode.Group(
        target.toPPNode(),
        Text(" = "),
        value.toPPNode(),
        Text(";"),
    )
    is CNode.LocalDecl -> PPNode.Group(
        type.toPPNode(),
        Text(" "),
        Text(name),
        Text(";"),
    )

    is CNode.Call -> PPNode.Group(
        target.toPPNode(),
        PPNode.Indent(
            Text("("),
            PPNode.Indent(
                args.mapIndexed { idx, it ->
                    it.toPPNode() + Text(if (idx == args.lastIndex) "" else ", ")
                }
            ),
            Text(");")
        )
    )

    is CNode.Return -> PPNode.Group(
        Text("return "),
        value.toPPNode(),
        Text(";")

    )
    is CNode.AddressOf -> PPNode.Group(
        Text("&"),
        target.toPPNode()
    )
    is CNode.DeclAssign -> PPNode.Group(
        type.toPPNode(),
        Text(" "),
        Text(name),
        Text(" = "),
        value.toPPNode(),
        Text(";")
    )
    is CNode.Dot -> PPNode.Nodes(
        lhs.toPPNode(),
        PPNode.LineIfWrapping,
        Text("."),
        Text(rhs)
    )
}

fun declarationsToPPNode(declarations: List<CNode>): PPNode {
    return PPNode.Group(
        declarations.mapIndexed { idx, it ->
            it.toPPNode() + Text("\n") +
                if (idx == declarations.lastIndex) Text("") else Text("\n")
        }
    )
}