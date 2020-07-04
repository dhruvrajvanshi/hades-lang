package hadesc.irgen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.typer.ImplementationBinding
import hadesc.typer.PropertyBinding
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.ir.*
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type
import java.util.*

