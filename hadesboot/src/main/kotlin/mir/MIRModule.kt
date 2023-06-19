package mir

data class MIRModule(val values: Map<String, MIRValue>)

class MIRModuleBuilder {
    private val values = mutableMapOf<String, MIRValue>()
    private val types = mutableMapOf<String, MIRType>()

    fun addValue(name: String, value: MIRValue) {
        values[name] = value
    }

    fun addType(name: String, type: MIRType) {
        types[name] = type
    }

    internal fun build(): MIRModule = MIRModule(values)
}

fun buildModule(run: MIRModuleBuilder.() -> Unit): MIRModule {
    val builder = MIRModuleBuilder()

    builder.run()
    return builder.build()
}
