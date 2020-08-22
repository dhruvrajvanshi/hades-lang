package hadesc

import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.logging.logger
import java.nio.file.Path
import kotlin.streams.toList

sealed class Options {

    companion object {

        fun fromArgs(args: Array<String>): Options {
            val output = Path.of(args.getString("--output"))
            val runtime = Path.of(args.getString("--runtime"))
            val main = Path.of(args.getString("--main"))
            val lib = args.getBool("--lib")
            val directories = args.getList("--directories").map { Path.of(it) }
            val cFlags = if (args.contains("--cflags")) {
                args.getList("--cflags")
            } else {
                emptyList()
            }
            val libs = args.getMany("-l")
            val cSources = args.getList("--c-sources").map { Path.of(it) }
            val debugSymbols = args.contains("-g")
            directories.forEach {
                assert(it.toFile().exists())
            }
            return BuildOptions(
                directories = directories,
                output = output,
                main = main,
                runtime = runtime,
                cFlags = cFlags,
                debugSymbols = debugSymbols,
                cSources = cSources,
                lib = lib,
                dumpLLVMModule = args.getBool("--dump-llvm-module"),
                libs = libs
            )
        }

        private fun Array<String>.getString(long: String): String {
            assert(indexOf(long) > -1) { "Missing flag $long" }
            val indexOfNext = indexOf(long) + 1
            assert(indexOfNext < size)
            assert(!this[indexOfNext].startsWith("--"))
            return this[indexOfNext]
        }

        private fun Array<String>.getBool(long: String): Boolean {
            return indexOf(long) > -1
        }

        private fun Array<String>.getList(long: String): List<String> {
            assert(indexOf(long) > -1) { "Missing flag $long" }
            return if (indexOf(long) == size - 1) {
                listOf()
            } else {
                toList().stream().skip(indexOf(long) + 1L).takeWhile { !it.startsWith("--") }.toList()
            }
        }

        private fun Array<String>.getMany(flag: String): List<String> {
            return this.filter { it.startsWith(flag) }.map { it.replaceFirst(flag, "")  }
        }
    }
}

data class BuildOptions(
    val directories: List<Path>,
    val output: Path,
    val main: Path,
    val runtime: Path,
    val cFlags: List<String>,
    val debugSymbols: Boolean,
    val cSources: List<Path>,
    val lib: Boolean,
    val dumpLLVMModule: Boolean,
    val libs: List<String>
) : Options()

class Compiler(
    args: Array<String>
) {
    private val log = logger()
    private val options = Options.fromArgs(args)

    fun run(): List<Diagnostic> {
        log.debug("CompilerOptions: $options")
        return when (options) {
            is BuildOptions ->
                build(options)
        }
    }

    private fun build(options: BuildOptions): List<Diagnostic> {
        val ctx = Context(options)
        log.debug("Building")
        ctx.build()
        return ctx.diagnosticReporter.errors
    }
}
