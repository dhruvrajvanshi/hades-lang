package hadesc

import hadesc.logging.logger
import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.*
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import kotlin.test.assertEquals

class HadesTestSuite {

    @TestFactory
    fun `should run test suite`(): List<DynamicNode> {
        val directory = File("test")
        assert(directory.exists())
        assert(directory.isDirectory)
        val utilsCLib = Paths.get(directory.toString(), "submodule", "test_utils.c")
        val outputDirectory = Path.of("test_build").toFile()
        if (outputDirectory.exists()) {
            outputDirectory.deleteRecursively()
        }
        outputDirectory.mkdirs()
        val files = directory.listFiles() ?: arrayOf()

        return files
            .sortedBy { it.name }
            .filter { it.extension == "hds" }
            .map { file -> DynamicTest.dynamicTest(file.nameWithoutExtension, file.toURI()) {
                val expectedStdoutFile = Paths.get(
                    directory.toPath().toString(),
                    file.nameWithoutExtension + ".stdout"
                ).toFile()
                val expectedErrorsFile = Paths.get(
                    directory.toPath().toString(),
                    file.nameWithoutExtension + ".errors"
                ).toFile()
                val cSourceFile = Paths.get(
                    directory.toPath().toString(),
                    file.nameWithoutExtension + ".c"
                ).toFile()

                logger().debug("Running suite file {}", file)

                val outputPath = Paths.get(
                    outputDirectory.toString(),
                    if (SystemUtils.IS_OS_WINDOWS)
                        file.nameWithoutExtension + ".exe"
                    else
                        file.nameWithoutExtension
                )
                val cSources = if (cSourceFile.exists()) {
                    cSourceFile.toString()
                } else {
                    ""
                }
                val compiler = Compiler(
                    arrayOf(
                        "--output", outputPath.toString(),
                        "--directories", directory.toString(),
                        "--main", file.toString(),
                        "--c-sources", cSources,
                        "--cflags", utilsCLib.toString(), "-D", "DEBUG"
                    )
                        .filter { it.isNotBlank() }
                        .toTypedArray()
                )
                val diagnostics = compiler.run()
                if (expectedStdoutFile.exists()) {
                    assert(File(outputPath.toUri()).exists()) {
                        "Expected $outputPath to be present after compilation"
                    }
                    val actualStdoutFile =
                        Path.of(outputDirectory.toString(), file.nameWithoutExtension + ".stdout")
                            .toFile()

                    val process = ProcessBuilder(outputPath.toString())
                        .redirectError(ProcessBuilder.Redirect.INHERIT)
                        .redirectOutput(actualStdoutFile)
                        .start()
                    val hasExited = process.waitFor(3, TimeUnit.SECONDS)
                    if (!hasExited) {
                        process.destroy()
                    }
                    assert(hasExited)
                    print(actualStdoutFile.readText())
                    assert(process.exitValue() == 0) {
                        "Test program exited with non-zero exit code"
                    }
                    val expectedLines = expectedStdoutFile.readLines()
                    val actualLines = actualStdoutFile.readLines()
                    assertEquals(
                        expectedLines, actualLines,
                        "Contents of $expectedStdoutFile and $actualStdoutFile don't match"
                    )
                } else {
                    assert(expectedErrorsFile.exists())
                    val expectedErrors = expectedErrorsFile.readLines().joinToString("\n")
                    val actualErrors = diagnostics
                        .sortedBy { it.sourceLocation.start }
                        .joinToString("\n") {
                            "${
                                it.sourceLocation.file.path.toString().replace('\\', '/')
                            }:${it.sourceLocation.start.line}: ${it.kind::class.simpleName}"
                        }
                    Assertions.assertEquals(
                        expectedErrors,
                        actualErrors
                    )
                }

            }
        }
    }
}