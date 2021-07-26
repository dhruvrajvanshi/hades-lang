package hadesc

import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.logging.logger
import java.nio.file.Path
import kotlin.system.exitProcess

sealed class Options {

    companion object {

        fun fromArgs(args: Array<String>): Options {
            val hadesHome = System.getenv("HADES_HOME")
            if (hadesHome == null) {
                System.err.println("Environment HADES_HOME must be set to a valid hades install location")
                exitProcess(1)
            }
            val stdlibFile = Path.of(hadesHome, "stdlib").toFile()
            if (!stdlibFile.exists() || !stdlibFile.isDirectory) {
                System.err.println(
                    "HADES_HOME must point to a valid hades installation\n" +
                            "hint: Directory must contain bin/hades and stdlib directory")
                exitProcess(1)
            }
            val output = Path.of(args.getString("--output"))
            val main = Path.of(args.getString("--main"))
            val lib = args.getBool("--lib")
            val directories = args.getList("--directories").map { Path.of(it) } +
                    listOf(Path.of(hadesHome, "stdlib"))
            val runtime = Path.of(hadesHome, "stdlib", "runtime.c")
            val cFlags = if (args.contains("--cflags")) {
                args.getList("--cflags")
            } else {
                emptyList()
            }
            val libs = args.getMany("-l")
            val cSources = args.getList("--c-sources").map { Path.of(it) } +
                    listOf(Path.of(hadesHome, "stdlib", "libc.c"))
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
                libs = libs,
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
            return if (indexOf(long) < 0 || indexOf(long) == size - 1) {
                listOf()
            } else {
                asSequence().drop(indexOf(long) + 1).takeWhile { !it.startsWith("--") }.toList()
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
    val libs: List<String>,
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
