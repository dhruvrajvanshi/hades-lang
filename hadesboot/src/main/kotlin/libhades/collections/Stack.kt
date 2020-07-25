package libhades.collections


class Stack<T> {
    private val items = mutableListOf<T>()

    fun push(value: T) {
        items.add(value)
    }

    @OptIn(ExperimentalStdlibApi::class)
    fun pop(): T {
        require(!isEmpty()) { "EmptyStack" }
        return items.removeLast()
    }

    fun peek(): T? = items.lastOrNull()

    val size = items.size
    fun isEmpty() = items.isEmpty()

    operator fun iterator() = items.asReversed().iterator()
}