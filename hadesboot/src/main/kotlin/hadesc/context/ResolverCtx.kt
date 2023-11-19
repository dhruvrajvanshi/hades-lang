package hadesc.context

import hadesc.resolver.Resolver

interface ResolverCtx {
    val resolver: Resolver<*>
}