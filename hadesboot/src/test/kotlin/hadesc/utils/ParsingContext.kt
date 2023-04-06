package hadesc.utils

import hadesc.Name
import hadesc.context.Context
import hadesc.unit
import io.mockk.every
import io.mockk.mockk

val mockParsingContext = mockk<Context> {
    every { resolver } returns mockk {
        every { onParseBlock(any()) } returns unit
        every { onParseSourceFile(any()) } returns unit
        every { onParseDeclaration(any()) } returns unit
        every { onParseClosure(any()) } returns unit
        every { onParseMatchArm(any()) } returns unit
        every { onParseMatchExpression(any()) } returns unit
        every { onParseScopeNode(any()) } returns unit
    }
    every { makeName(any()) } answers { Name(invocation.args[0] as String) }
}