package hades.test

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitClause
import hadesc.analysis.TraitResolver
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import org.junit.jupiter.api.Test
import java.nio.file.Path

class TraitResolverTest {
    @Test
    fun `should resolve impls without where clauses`() {
        val resolver = TraitResolver(TraitResolver.Env(
                impl(params(), qn("Printable"), args(Type.Bool), requirements())
        ))
        assert(resolver.isTraitImplemented(qn("Printable"), args(Type.Bool)))
    }

    @Test
    fun `should resolve impls with type params`() {
        val self = param("T")
        val resolver = TraitResolver(
                TraitResolver.Env(
                        impl(params(self), qn("Printable"), args(self.ref), requirements())
                )
        )

        assert(resolver.isTraitImplemented(qn("Printable"), args(Type.Bool)))
        assert(resolver.isTraitImplemented(qn("Printable"), args(Type.Integral(32, isSigned = false))))
    }

    private var currentColumn = 1
    private fun param(name: String): Type.Param {
        val line = 1
        val column = currentColumn
        currentColumn++
        return Type.Param(Binder(
                Identifier(SourceLocation(
                        file = SourcePath(Path.of("test.hds")),
                        start = Position(line, column),
                        stop = Position(line, column)
                ),  Name(name))
        ))
    }

    private fun params(vararg params: Type.Param): List<Type.Param> {
        return listOf(*params)
    }

    private fun impl(params: List<Type.Param>, traitRef: QualifiedName, arguments: List<Type>, requirements: List<TraitRequirement>): TraitClause.Implementation {
        return TraitClause.Implementation(params, traitRef, arguments, requirements)
    }

    private fun args(vararg types: Type): List<Type> {
        return listOf(*types)
    }

    private fun requirements(vararg requirements: TraitRequirement): List<TraitRequirement> {
        return listOf(*requirements)
    }

    private fun qn(name: String): QualifiedName {
        return QualifiedName(listOf(Name(name)))
    }


}