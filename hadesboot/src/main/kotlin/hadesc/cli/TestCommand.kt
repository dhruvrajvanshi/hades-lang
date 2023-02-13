package hadesc.cli

import com.github.ajalt.clikt.completion.CompletionCandidates
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.option

class TestCommand : CliktCommand(name = "test") {
    val file = option(completionCandidates = CompletionCandidates.Path)
    override fun run() {
        TODO("Not yet implemented")
    }
}
