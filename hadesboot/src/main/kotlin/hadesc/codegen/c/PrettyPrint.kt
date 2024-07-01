package hadesc.codegen.c

import hadesboot.prettyprint.PPNode
import hadesboot.prettyprint.PPNode.*

fun CNode.toPPNode(): PPNode = when (this) {
    is CNode.Include -> Text("#include \"$path\"")
    is CNode.Raw -> Text(code)
    is CNode.PtrType -> Text(if (isConst) "const " else "") + type.toPPNode() + Text("*")
    is CNode.FnSignature -> Nodes(
        if (isExtern) Text("extern ") else Text(""),
        returnType.toPPNode(),
        Text(" "),
        Text(name),
        Group(
            Text("("),
            LineIfWrapping,
            Indent(
                parameters.mapIndexed { index, it ->
                    it.toPPNode() + (if (index == parameters.lastIndex) Text("") else Text(", ")) +
                            LineIfWrapping
                }
            ),
            LineIfWrapping,
            Text(")"),
        ),
        Text(";")
    )

    is CNode.ExternConst -> {
        Group(
            Text("extern"),
            SpaceOrLine,
            type.toPPNode(),
            SpaceOrLine,
            Text(name),
            Text(";")
        )
    }

    is CNode.StructDef -> {
        Nodes(
            Text("typedef struct $name"),
            Group(
                Text("{"),
                LineIfWrapping,
                Indent(
                    fields.map {
                        it.second.toPPNode() + Text(" ") + Text(it.first) + Text(";")
                    }
                ),
                LineIfWrapping,
                Text("} $name;")
            )
        )
    }

    is CNode.UnionDecl -> {
        Nodes(
            Text("typedef union ${name.c()}"),
            Group(
                Text("{"),
                LineIfWrapping,
                Indent(
                    members.mapIndexed { idx, it ->
                        it.toPPNode() + Text(" ") + Text("_$idx") + Text(";")
                    }
                ),
                LineIfWrapping,
                Text("} ${name.c()};")
            )
        )
    }

    is CNode.TypedefFnPtr -> Group(
        Text("typedef "),
        returnType.toPPNode(),
        Text(" (*${name.c()})"),
        Text("("),
        Indent(
            parameters.mapIndexed { idx, it ->
                it.toPPNode() + Text(if (idx == parameters.lastIndex) "" else ", ")
            }
        ),
        Text(");")

    )

    is CNode.FnDefinition -> Nodes(
        returnType.toPPNode(),
        Text(" "),
        Text(name),
        Group(
            Text("("),
            LineIfWrapping,
            Indent(
                Nodes(parameters.mapIndexed { idx, (name, ty) ->
                    ty.toPPNode() + SpaceOrLine + Text(name) + Text(if (idx == parameters.lastIndex) "" else ", ") + LineIfWrapping
                }),
            ),
            LineIfWrapping,
            Text(")"),
        ),
        SpaceOrLine,
        body.toPPNode()

    )

    is CNode.ConstDef -> Group(
        type.toPPNode(),
        Text(" "),
        Text(name),
        Text("="),
        Indent(
            SpaceOrLine,
            initializer.toPPNode(),
        ),
        Text(";")
    )

    is CNode.FunctionBody -> Group(
        Text("{"),
        LineIfWrapping,
        Text("// prelude\n"),
        Indent(
            prelude.map { it.toPPNode() + LineIfWrapping },
        ),
        Text("// end prelude\n"),
        Nodes(items.map {
            it.toPPNode() + LineIfWrapping
        }),
        LineIfWrapping,
        Text("}"),
    ).forceWrap()

    is CNode.Assign -> Nodes(
        target.toPPNode(),
        Text(" ="),
        Group(
            SpaceOrLine,
            Indent(
                value.toPPNode(),
            ),
        ),
        Text(";"),
    )

    is CNode.LocalDecl -> Nodes(
        type.toPPNode(),
        Text(" "),
        Text(name),
        Text(";"),
    )

    is CNode.Call -> Nodes(
        target.toPPNode(),
        Group(

            Text("("),
            Indent(
                Indent(
                    args.mapIndexed { idx, it ->
                        it.toPPNode() + Text(if (idx == args.lastIndex) "" else ", ") + LineIfWrapping
                    }
                ),
            ),
            Text(")")
        ),
        Text(";")
    )

    is CNode.Return -> Nodes(
        Group(
            Text("return"),
            SpaceOrLine,
            IfWrap(Text("("), Text("")),
            Indent(
                value.toPPNode(),
            ),
            IfWrap(Text(")"), Text(""))
        ),
        Text(";")

    )

    is CNode.AddressOf -> Nodes(
        Text("&"),
        target.toPPNode()
    )

    is CNode.Dot -> Nodes(
        lhs.toPPNode(),
        LineIfWrapping,
        Text("."),
        Text(rhs)
    )
    is CNode.Arrow -> Nodes(
        lhs.toPPNode(),
        LineIfWrapping,
        Text("->"),
        Text(rhs)
    )

    is CNode.Prefix -> Nodes(
        Text(op),
        Text("("),
        value.toPPNode(),
        Text(")"),
    )
    is CNode.LabeledStatements -> Group(
        Text("$label:"),
        Indent(
            statements.map { it.toPPNode() + LineIfWrapping }
        ),
        LineIfWrapping
    ).forceWrap()

    is CNode.RawPP -> node
    is CNode.Deref -> Group(
        Text("*"),
        Text("("),
        Indent(
            LineIfWrapping,
            node.toPPNode(),
            LineIfWrapping,
        ),
        Text(")"),
    )

    is CNode.DefaultCase -> Nodes(
        Text("default: "),
        body.toPPNode()
    )
    is CNode.Goto -> Nodes(
        Text("goto $label;")
    )
    is CNode.Switch -> Nodes(
        Text("switch"),
        Group(
            Text("("),
            LineIfWrapping,
            Indent(
                expr.toPPNode(),
            ),
            LineIfWrapping,
            Text(")")
        ),
        Group(
            Text(" {"),
            LineIfWrapping,
            Nodes(cases.map { it.toPPNode() + LineIfWrapping }),
            Text("}")
        ).forceWrap()
    )
    is CNode.SwitchCase -> Nodes(
        Text("case "),
        value.toPPNode(),
        Text(": "),
        body.toPPNode()

    )

    is CNode.BraceInitializedList -> Group(
        Text("{"),
        Indent(
            LineIfWrapping,
            Nodes(
                values.mapIndexed { idx, it ->
                    it.toPPNode() + Text(if (idx == values.lastIndex) "" else ", ") + LineIfWrapping
                }
            ),
            LineIfWrapping,
        ),
        Text("}"),
    )
    is CNode.Cast -> Group(
        Text("("),
        Indent(
            LineIfWrapping,
            toType.toPPNode(),
            LineIfWrapping,
        ),
        Text(") "),
        value.toPPNode()
    )
}

fun declarationsToPPNode(declarations: List<CNode>): PPNode {
    return Group(
        declarations.mapIndexed { idx, it ->
            it.toPPNode() + Text("\n") +
                    if (idx == declarations.lastIndex) Text("") else Text("\n")
        }
    )
}