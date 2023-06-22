package mir

import java.nio.file.Path

data class MIRModule(val declarations: List<MIRDeclaration>)

class MIRModuleBuilder(val path: Path) {
    private val declarations = mutableListOf<MIRDeclaration>()

    fun addFunction(name: String, returnType: MIRType, buildFunction: MIRFunctionBuilder.() -> Unit) {
        val f = buildFunction(name, path, returnType) {
            buildFunction()
        }

        addDeclaration(f)
    }

    fun addStatic(name: String, value: MIRValue) {
        addDeclaration(
            MIRDeclaration.StaticDefinition(
                name,
                value.type,
                value,
            )
        )
    }

    internal fun getStaticDef(name: String): MIRDeclaration.StaticDefinition? {
        return declarations
            .filterIsInstance<MIRDeclaration.StaticDefinition>()
            .find { it.name == name }
    }

    private fun addDeclaration(declaration: MIRDeclaration) {
        declarations.add(declaration)
    }

    internal fun build(): MIRModule = MIRModule(declarations)
}

fun buildModule(path: Path, run: MIRModuleBuilder.() -> Unit): MIRModule {
    val builder = MIRModuleBuilder(path)

    builder.run()
    return builder.build()
}

fun buildModule(path: String, run: MIRModuleBuilder.() -> Unit): MIRModule {
    return buildModule(Path.of(path), run)
}