package libhades.collections

class Stack<T> {
    private val items = mutableListOf<T>()

    fun push(value: T) {
        items.add(value)
    }

    fun pop(): T {
        require(!isEmpty()) { "EmptyStack" }
        return items.removeLast()
    }

    fun peek(): T? = items.lastOrNull()

    val size = items.size
    private fun isEmpty() = items.isEmpty()

    operator fun iterator() = items.asReversed().iterator()

    fun items(): List<T> = items
}
