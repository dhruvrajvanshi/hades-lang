package hadesc

import hadesc.cli.BuildCommand
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import java.io.File
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import kotlin.io.path.exists
import kotlin.io.path.nameWithoutExtension

class StdlibTests {
    @TestFactory
    fun runStdlibTests() = discoverTests().map { testSource ->
        DynamicTest.dynamicTest(testSource.nameWithoutExtension, testSource.toUri()) {
            val output = "test_build/${testSource.nameWithoutExtension}"
            val compiler = BuildCommand()
            compiler.main(
                arrayOf(
                    "--main", testSource.toString(),
                    "--output", output,
                    "--internal-skip-exec",
                    "-g"
                )
            )
            assert(compiler.execute().isEmpty())
            assert(Path.of(output).exists())

            val process = ProcessBuilder(output)
                .redirectError(ProcessBuilder.Redirect.INHERIT)
                .redirectOutput(ProcessBuilder.Redirect.INHERIT)
                .start()
            val hasExited = process.waitFor(3, TimeUnit.SECONDS)
            if (!hasExited) {
                process.destroy()
            }
            assert(hasExited)
            assert(process.exitValue() == 0) {
                "Test program exited with non-zero exit code"
            }
        }
    }.toList()

    private fun discoverTests() = sequence<Path> {
        for (file in File("stdlib").walkTopDown()) {
            if (file.extension != "hds") {
                continue
            }
            if (file.nameWithoutExtension.endsWith("_test")) {
                yield(file.toPath())
            }
        }
    }

}