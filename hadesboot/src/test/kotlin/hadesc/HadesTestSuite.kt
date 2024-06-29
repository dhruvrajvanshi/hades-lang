package hadesc

import hadesc.cli.BuildCommand
import hadesc.logging.logger
import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.*
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import kotlin.test.assertEquals

class HadesTestSuite {
    private val log = logger(HadesTestSuite::class.java)

    @TestFactory
    fun `should run test suite`(): List<DynamicNode> {
        val directory = File("test")
        assert(directory.exists())
        assert(directory.isDirectory)
        val utilsCLib = Paths.get(directory.toString(), "packages", "submodule", "test_utils.c")
        val outputDirectory = Path.of("test_build").toFile()
        if (outputDirectory.exists()) {
            outputDirectory.deleteRecursively()
        }
        outputDirectory.mkdirs()
        val files = directory.listFiles() ?: arrayOf()

        return files
            .sortedBy { it.name }
            .filter { it.extension == "hds" }
            .map { file ->
                DynamicTest.dynamicTest(file.nameWithoutExtension, file.toURI()) {
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

                    log.debug("Running suite file {}", file)

                    val outputPath = Paths.get(
                        outputDirectory.toString(),
                        if (SystemUtils.IS_OS_WINDOWS) {
                            file.nameWithoutExtension + ".exe"
                        } else {
                            file.nameWithoutExtension
                        }
                    )
                    val compiler = BuildCommand()
                    val flags = mutableListOf(
                        "--output", outputPath.toString(),
                        "--module-path", Paths.get(directory.toString(), "packages").toString(),
                        "--main", file.toString(),
                        "--enable-hir-verifier",
                        "--internal-skip-exec",
                        "--json-diagnostics",
                        "--backend", "C",
                    ).apply {
                        add("--c-source")
                        add(utilsCLib.toString())
                        if (cSourceFile.exists()) {
                            add("--c-source")
                            add(cSourceFile.toString())
                        }
                    }
                    compiler.main(flags)
                    val diagnostics = compiler.execute()
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
                        val expectedLines = expectedStdoutFile.readLines().map {
                            if (it.startsWith("#re/")) {
                                check(it.endsWith("/"))
                                val pattern = it.removePrefix("#re/").removeSuffix("/")
                                it to Regex(pattern)
                            } else {
                                it to Regex(Regex.escape(it))
                            }
                        }
                        val actualLines = actualStdoutFile.readLines()

                        expectedLines.zip(actualLines).forEach { (expectation, actualText) ->
                            val (patternText, expectedPattern) = expectation
                            assert(expectedPattern.matches(actualText)) {
                                "$actualText doesn't match pattern ${patternText.removePrefix("#re")}"
                            }
                        }
                        assertEquals(
                            expectedLines.size,
                            actualLines.size,
                            "Expected ${expectedLines.size} lines, found ${actualLines.size}"
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
