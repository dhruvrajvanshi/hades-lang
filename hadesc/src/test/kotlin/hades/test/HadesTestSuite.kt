package hades.test

import hadesc.Compiler
import hadesc.logging.logger
import org.junit.jupiter.api.Test
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import kotlin.test.assertEquals

class HadesTestSuite {
    @Test
    fun `should run test suite`() {
        val directory = File("suite")
        val outputDirectory = Path.of("suite_build")
        File(outputDirectory.toUri()).mkdirs()
        for (file in directory.listFiles() ?: arrayOf()) {
            if (file.extension == "hds") {
                logger().debug("Running suite file {}", file)
                val expectedStdoutFile = Paths.get(
                    directory.toPath().toString(),
                    file.nameWithoutExtension + ".stdout"
                )
                    .toFile()
                assert(expectedStdoutFile.exists())

                val outputPath = Paths.get(outputDirectory.toString(), file.nameWithoutExtension)

                Compiler(
                    arrayOf(
                        "--output", outputPath.toString(),
                        "--directories", "stdlib", directory.toString(),
                        "--main", file.toString(),
                        "--runtime", "runtime.c"
                    )
                ).run()
                assert(File(outputPath.toUri()).exists()) {
                    "Expected $outputPath to be present after compilation"
                }
                val actualStdoutFile = Path.of(outputDirectory.toString(), file.nameWithoutExtension + ".stdout")
                    .toFile()

                val process = ProcessBuilder(outputPath.toString())
                    .redirectError(ProcessBuilder.Redirect.INHERIT)
                    .redirectOutput(actualStdoutFile)
                    .start()
                process.waitFor(1, TimeUnit.SECONDS)
                assert(process.exitValue() == 0)
                assertEquals(
                    expectedStdoutFile.readText(), actualStdoutFile.readText(),
                    "Contents of $expectedStdoutFile and $actualStdoutFile don't match"
                )

            }
        }
    }
}