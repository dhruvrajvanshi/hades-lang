package hadesc

import hadesc.context.Context
import hadesc.logging.logger
import java.nio.file.Path
import kotlin.streams.toList

sealed class Options {

    companion object {

        fun fromArgs(args: Array<String>): Options {
            val output = Path.of(args.getString("--output"))
            val runtime = Path.of(args.getString("--runtime"))
            val main = Path.of(args.getString("--main"))
            val directories = args.getList("--directories").map { Path.of(it) }
            directories.forEach {
                assert(it.toFile().exists())
            }
            return BuildOptions(
                directories = directories,
                output = output,
                main = main,
                runtime = runtime
            )
        }

        private fun Array<String>.getString(long: String): String {
            assert(indexOf(long) > -1) { "Missing flag $long" }
            val indexOfNext = indexOf(long) + 1
            assert(indexOfNext < size)
            assert(!this[indexOfNext].startsWith("--"))
            return this[indexOfNext]
        }

        private fun Array<String>.getList(long: String): List<String> {
            assert(indexOf(long) > -1) { "Missing flag $long" }
            return if (indexOf(long) == size - 1) {
                listOf()
            } else {
                toList().stream().skip(indexOf(long) + 1L).takeWhile { !it.startsWith("--") }.toList()
            }
        }
    }
}

data class BuildOptions(
    val directories: List<Path>,
    val output: Path,
    val main: Path,
    val runtime: Path
) : Options()

class Compiler(
    args: Array<String>
) {
    private val log = logger()
    private val options = Options.fromArgs(args)

    fun run() {
        log.debug("CompilerOptions: $options")
        when (options) {
            is BuildOptions ->
                build(options)
        }
    }

    private fun build(options: BuildOptions) {
        log.debug("Building")
        val ctx = Context(options)
        ctx.build()
    }
}
