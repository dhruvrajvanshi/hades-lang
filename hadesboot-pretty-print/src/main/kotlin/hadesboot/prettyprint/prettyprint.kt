/**
 * Shamelessly copied from https://yorickpeterse.com/articles/how-to-write-a-code-formatter/,
 * including some of the comments.
 * Thanks, Yorick Peterse!
 */
package hadesboot.prettyprint

sealed interface PPNode {
    data class Text(val text: String) : PPNode

    /**
     * Renders to a space if it resides in a group that doesn't need wrapping,
     * and renders to a line when wrapping is needed.
     */
    data object SpaceOrLine : PPNode

    /**
     * Renders to a new line if it resides in a group that needs wrapping,
     * otherwise it renders to nothing.
     */
    data object LineIfWrapping : PPNode

    /**
     * Renders one or more [nodes], indenting each new line, but only if it resides
     * in a group that for which wrapping is needed.
     */
    data class Indent(val nodes: List<PPNode>) : PPNode {
        constructor(vararg nodes: PPNode) : this(nodes.toList())
    }

//    data class ForceIndent(val nodes: List<PPNode>) : PPNode {
//        constructor(vararg nodes: PPNode) : this(nodes.toList())
//    }

    /**
     * Renders one or more [nodes], without any special formatting.
     * Think of this as a simple concatenation operator.
     */
    data class Nodes(val nodes: List<PPNode>) : PPNode {
        constructor(vararg nodes: PPNode) : this(nodes.toList())
    }

    /**
     * Renders many [nodes] into a single line if it can fit in the current line,
     * otherwise into multiple lines.
     */
    data class Group(val nodes: List<PPNode>, val force: Boolean = false) : PPNode {
        constructor(vararg nodes: PPNode) : this(nodes.toList())
        fun forceWrap(): Group = Group(nodes, true)
    }

    /**
     * Renders [ifTrue] if the closest ancestor [PPNode.Group] needs to be wrapped, otherwise [ifFalse].
     */
    data class IfWrap(val ifTrue: PPNode, val ifFalse: PPNode) : PPNode

    operator fun plus(other: PPNode): PPNode = Nodes(listOf(this, other))
}

private enum class Wrapping {
    ENABLE,
    DETECT
}

private val Wrapping.enabled get() = this == Wrapping.ENABLE

private fun PPNode.minWidth(wrapping: Wrapping): Int = when (this) {
    is PPNode.Text -> text.length
    is PPNode.SpaceOrLine -> 1
    is PPNode.LineIfWrapping -> 0
    is PPNode.Indent -> nodes.sumOf { it.minWidth(wrapping) }
    is PPNode.Nodes -> nodes.sumOf { it.minWidth(wrapping) }
    is PPNode.Group -> nodes.sumOf { it.minWidth(wrapping) }
    is PPNode.IfWrap ->
        if (wrapping.enabled)
            ifTrue.minWidth(wrapping)
        else
            ifFalse.minWidth(wrapping)
}

private class Renderer(private val config: PrettyPrintConfig) {
    private val buffer = StringBuilder()
    private var indent = 0;
    private var size = 0
    private val wrapped = mutableSetOf<Int>()

    fun render(node: PPNode): String {
        generate(node)
        val result = buffer.toString()
        reset()
        return result
    }

    fun generate(node: PPNode) {
        visitNode(node, Wrapping.DETECT)
    }

    fun visitNode(node: PPNode, wrapping: Wrapping): Unit = when (node) {
        is PPNode.Text -> text(node.text)

        PPNode.LineIfWrapping -> if (wrapping.enabled) newLine() else Unit

        PPNode.SpaceOrLine -> if (wrapping.enabled) newLine() else text(" ")

        is PPNode.Nodes -> node.nodes.forEach { visitNode(it, wrapping) }

        is PPNode.Indent ->
            if (wrapping.enabled) {
                size += config.indent.length
                indent += 1
                buffer.append(config.indent)
                node.nodes.forEach { visitNode(it, wrapping) }
                indent -= 1
            } else {
                node.nodes.forEach { visitNode(it, wrapping) }
            }

        is PPNode.IfWrap ->
            if (wrapping.enabled) visitNode(node.ifTrue, Wrapping.ENABLE)
            else visitNode(node.ifFalse, Wrapping.DETECT)

        is PPNode.Group -> {
            val minWidth = node.nodes.sumOf { it.minWidth(wrapping) }
            val wrap = if (minWidth > config.lineWidth || node.force) {
                Wrapping.ENABLE
            } else {
                Wrapping.DETECT
            }
            node.nodes.forEach { visitNode(it, wrap) }
        }
    }

    private fun text(s: String) {
        size += s.length
        buffer.append(s)
        Unit
    }

    private fun newLine() {
        buffer.append("\n")
        size = 0
        repeat(indent) {
            buffer.append(config.indent)
            size += config.indent.length
        }
    }

    private fun reset() {
        buffer.clear();
        size = 0
        indent = 0;
        wrapped.clear()
    }
}

data class PrettyPrintConfig(val lineWidth: Int, val indent: String)

fun PPNode.prettyPrint(
    lineWidth: Int = 80,
    indent: String = "  ",
): String =
    Renderer(
        PrettyPrintConfig(
            lineWidth = lineWidth,
            indent = indent,
        )
    ).render(this)
