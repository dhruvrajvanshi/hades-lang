package hadesc.text

internal object Config {
    var maxChunkSize: Int = 128
    var branchingFactor: Int = 64
}
sealed interface Text {
    val newlineCount: Int
    fun size(): Int
    fun insert(line: Int, column: Int, text: CharSequence): Text {
        check(line >= 0)
        check(column >= 0)
        val offset = offsetOf(line, column)
        TODO()
    }

    /**
     * Convert human-readable line/column offsets (1 based)
     * to an index (0 based)
     */
    fun offsetOf(line: Int, column: Int): Int

    private data class Leaf(val string: CharSequence) : Text {
        override val newlineCount = string.count { it == '\n' }
        override fun size(): Int = string.length
        override fun offsetOf(line: Int, column: Int): Int {
            check(line >= 0)
            var offset = 0
            var linesSkipped = 0
            // skip n-1 lines
            string.forEachIndexed { idx, c ->
                if (linesSkipped == line - 1) {
                    return@forEachIndexed
                }
                offset = idx
                if (c == '\n') {
                    linesSkipped++
                }
            }
            assert(linesSkipped == line - 1)
            offset += column
            check(offset >= 0)
            check(offset < string.length)
            return offset
        }
    }

    private data class Interior(val children: List<Text>) : Text {
        val size: Int = children.sumOf { it.size() }
        override val newlineCount = children.sumOf { it.newlineCount }
        override fun size(): Int = size
        override fun offsetOf(line: Int, column: Int): Int {
            TODO("Not yet implemented")
        }
    }

    companion object {
        val EMPTY: Text = Leaf("")

        fun from(charSequence: CharSequence): Text {

            // build up a Tree of nodes bottom up
            // The algorithm here is
            // 1. Break the initial text into n leaf nodes
            // 2. roots = leaf nodes are chunked into groups of branching factor and made into interior nodes
            // loop while roots is not empty
            //   take previous set of roots in chunks of branchingFactor and convert them into interior nodes


            /// Example for a chunk size of 2 and branching factor of 1
            ///  012345678901234567
            ///  leaves = ['01', '23', '45', '67', ...]
            ///  roots = [ Interior('01, '23', '45'), Interior('67', '89', '01'), ... ]
            ///  after 1 iteration
            ///  roots = [Interior(Interior('01, '23', '45'), Interior('67', '89', '01'), Interior('23, '45', '67')), ...]
            val leaves = charSequence.chunked(Config.maxChunkSize).map { Leaf(it) }
            var roots = leaves.chunked(Config.branchingFactor).map { Interior(it) }

            while (roots.size != 1) {
                roots = roots.chunked(Config.branchingFactor).map { Interior(it) }
            }
            return roots[0]
        }
    }
}
