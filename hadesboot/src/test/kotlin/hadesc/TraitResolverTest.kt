package hadesc

import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitClause
import hadesc.analysis.TraitResolver
import hadesc.analysis.TypeAnalyzer
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
        val resolver = makeResolver(
                impl(params(), qn("Printable"), forType(Type.Bool), requires())
        )
        assert(resolver.isTraitImplemented(qn("Printable"), forType(Type.Bool)))
    }

    @Test
    fun `should resolve impls with type params`() {
        val self = param("T")
        val resolver = makeResolver(
                impl(params(self), qn("Printable"), forType(self.ref), requires())
        )

        assert(resolver.isTraitImplemented(qn("Printable"), forType(Type.Bool)))
        assert(resolver.isTraitImplemented(qn("Printable"), forType(Type.Integral(32, isSigned = false))))
    }

    @Test
    fun `should check impl requirements`() {
        val t = param("T1")
        val resolver = makeResolver(
                // implementation : Printable[Bool]
                impl(params(), qn("Printable"), forType(Type.Bool), requires()),
                // implementation : Printable[i32]
                impl(params(), qn("Printable"), forType(Type.Integral(32, true)), requires()),

                // implementation [T] : Printable[Box[T]] where Printable[T]
                impl(
                        params(t), qn("Printable"), forType(
                            tycon("Box").ap(t.ref)
                        ),
                        requires(
                                requirement(qn("Printable"), t.ref)
                        )
                )
        )
        assert(resolver.isTraitImplemented(qn("Printable"), forType(Type.Bool)))
        assert(resolver.isTraitImplemented(qn("Printable"), forType(Type.Integral(32, true))))
        assert(!resolver.isTraitImplemented(qn("Printable"), forType(tycon("NonPrintable"))))
        assert(resolver.isTraitImplemented(qn("Printable"), forType(tycon("Box").ap(Type.Bool))))
        assert(resolver.isTraitImplemented(qn("Printable"),
                forType(tycon("Box").ap(Type.Integral(32, true)))))
        assert(!resolver.isTraitImplemented(qn("Printable"),
                forType(tycon("Box").ap(tycon("NonPrintable")))))
    }

    @Test
    fun `should check negated instances`() {
        // trait Foo[T]
        // trait Bar[T]
        // impl Foo[Bool]
        // impl[T] Bar[T] where not Foo[T]
        // isTraitImplemented(Bar[Bool]) == false
        // isTraitImplemented(Bar[isize]) == true

        val t = param("T")
        val resolver = makeResolver(
            impl(params(), qn("Foo"), forType(Type.Bool)),
            impl(
                params(t),
                qn("Bar"), forType(t.ref),
                requires(
                    TraitRequirement(
                        qn("Foo"),
                        listOf(t.ref),
                        negated = true
                    )
                )
            )
        )
        assert(resolver.isTraitImplemented(qn("Bar"), forType(Type.isize)))
        assert(!resolver.isTraitImplemented(qn("Bar"), forType(Type.Bool)))
    }

    private fun requirement(traitRef: QualifiedName, vararg args: Type): TraitRequirement {
        return TraitRequirement(traitRef, listOf(*args), negated = false)
    }

    private fun tycon(name: String): Type.Constructor {
        return Type.Constructor(qn(name))
    }

    private fun Type.Constructor.ap(vararg args: Type): Type.Application {
        return Type.Application(callee = this, args = listOf(*args))
    }

    private fun makeResolver(vararg clauses: TraitClause<Unit>): TraitResolver<Unit> {
        return TraitResolver(TraitResolver.Env(*clauses), TypeAnalyzer())
    }

    private var currentColumn = 1
    private fun param(name: String): Type.Param {
        val line = 1
        val column = currentColumn
        currentColumn++
        return Type.Param(Binder(
                Identifier(SourceLocation(
                        SourcePath(Path.of("test.hds")),
                        Position(line, column),
                        Position(line, column)
                ),  Name(name))
        ))
    }

    private fun params(vararg params: Type.Param): List<Type.Param> {
        return listOf(*params)
    }

    private fun impl(params: List<Type.Param>, traitRef: QualifiedName, arguments: List<Type>, requirements: List<TraitRequirement> = emptyList()): TraitClause.Implementation<Unit> {
        return TraitClause.Implementation(params, traitRef, arguments, requirements, def = unit)
    }

    private fun forType(vararg types: Type): List<Type> {
        return listOf(*types)
    }

    private fun requires(vararg requirements: TraitRequirement): List<TraitRequirement> {
        return listOf(*requirements)
    }

    private fun qn(name: String): QualifiedName {
        return QualifiedName(listOf(Name(name)))
    }


}