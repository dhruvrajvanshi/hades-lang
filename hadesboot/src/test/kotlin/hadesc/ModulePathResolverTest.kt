package hadesc

import org.junit.jupiter.api.Test
import java.nio.file.Paths
import kotlin.test.assertEquals

private val baseDir = Paths.get("hadesboot", "module_path_resolver_test")
class ModulePathResolverTest {

    @Test
    fun `createModuleMap should correctly resolve paths`() {
        val result = createModuleMap(
            { Name(it) },
            listOf(baseDir.resolve("root1"))
        )

        val actual = result
                .mapKeys { entry -> entry.key.names.joinToString(".") { it.text } }
                .mapValues { it.value.joinToString("/").replace("hadesboot/module_path_resolver_test/", "") }

        assertEquals(
            actual = actual,
            expected = mapOf(
                "a" to "root1/a.hds",
                "a.b" to "root1/a/b.hds",
                "a.b.c.d" to "root1/a/b/c/d.hds"
            )
        )
    }
}