package hades.test

import hadesc.Compiler
import hadesc.logging.logger
import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.DynamicNode
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import kotlin.test.assertEquals

class HadesTestSuite {

    @OptIn(ExperimentalStdlibApi::class)
    @TestFactory
    fun `should run test suite`(): List<DynamicNode> {
        val directory = File("suite")
        val utilsCLib = Paths.get(directory.toString(), "submodule", "test_utils.c")
        val outputDirectory = Path.of("suite_build").toFile()
        if (outputDirectory.exists()) {
            outputDirectory.deleteRecursively()
        }
        outputDirectory.mkdirs()
        val files = directory.listFiles() ?: arrayOf()
        return buildList {
            for (file in files) {
                if (file.extension == "hds") {
                    add(DynamicTest.dynamicTest(file.name) {
                        logger().debug("Running suite file {}", file)
                        val expectedStdoutFile = Paths.get(
                            directory.toPath().toString(),
                            file.nameWithoutExtension + ".stdout"
                        ).toFile()
                        val expectedErrorsFile = Paths.get(
                            directory.toPath().toString(),
                            file.nameWithoutExtension + ".errors"
                        ).toFile()

                        val outputPath = Paths.get(
                            outputDirectory.toString(),
                            if (SystemUtils.IS_OS_WINDOWS)
                                file.nameWithoutExtension + ".exe"
                            else
                                file.nameWithoutExtension
                        )
                        val compiler = Compiler(
                            arrayOf(
                                "--output", outputPath.toString(),
                                "--directories", "stdlib", directory.toString(),
                                "--main", file.toString(),
                                "--runtime", "runtime.c",
                                "--cflags", utilsCLib.toString()
                            )
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
                            process.waitFor(1, TimeUnit.SECONDS)
                            assert(process.exitValue() == 0)
                            val expectedLines = expectedStdoutFile.readLines()
                            val actualLines = actualStdoutFile.readLines()
                            assertEquals(
                                expectedLines, actualLines,
                                "Contents of $expectedStdoutFile and $actualStdoutFile don't match"
                            )
                        } else {
                            assert(expectedErrorsFile.exists())
                            val expectedErrors = expectedErrorsFile.readLines()
                            val actualErrors = diagnostics
                                    .sortedBy { it.sourceLocation.start }
                                    .map {
                                        "${it.sourceLocation.file.path}:${it.sourceLocation.start.line}: ${it.kind::class.simpleName}"
                                    }
                            assertEquals(
                                expectedErrors,
                                actualErrors
                            )
                        }

                    })
                }
            }
        }
    }
}