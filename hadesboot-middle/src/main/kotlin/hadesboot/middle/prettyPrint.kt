package hadesboot.middle

fun Module.prettyPrint(): String =
    items.joinToString("\n\n") { it.prettyPrint() }

fun Item.prettyPrint(): String = when (this) {
    is ExternFn -> {
        val params = parameters.joinToString(", ") { it.prettyPrint() }
        "fn $name($params) -> ${returnType.prettyPrint()}"
    }
    is Fn -> {
        val params = parameters.joinToString(", ") { it.prettyPrint() }
        "fn $name($params) -> ${returnType.prettyPrint()}"
    }
}

fun Type.prettyPrint(): String = when(this) {
    Type.USize -> "usize"
    Type.ISize -> "isize"
    Type.U32 -> "u32"
    Type.I32 -> "i32"
    is Type.Tuple ->
            members.joinToString( ", ", "(",  ")") { it.prettyPrint() }
}

fun Parameter.prettyPrint(): String = "$name: ${type.prettyPrint()}"