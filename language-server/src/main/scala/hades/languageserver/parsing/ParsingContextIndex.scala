package hades.languageserver.parsing

import cats.{Applicative, Monad}
import cats.syntax.all._
import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import hades.languageserver.lsp.{LSPRequest, LSPRequestParams}
import hades.languageserver.lsp.Range

trait ParsingContextIndex[F[_]] {
  def onDocumentOpen(params: LSPRequestParams.TextDocumentDidOpen): F[Unit]
  def onDocumentEdit(params: LSPRequestParams.TextDocumentDidChange): F[Unit]
}

case class URI(value: String) extends AnyVal
case class State(
  lines: Map[URI, Vector[String]] = Map()
)
class ParsingContextIndexImpl[F[_]: Sync: Monad](
  private val state: Ref[F, State]
) extends ParsingContextIndex[F] {

  override def onDocumentEdit(params: LSPRequestParams.TextDocumentDidChange): F[Unit] = {
    ().pure[F]
  }

  override def onDocumentOpen(params: LSPRequestParams.TextDocumentDidOpen): F[Unit] = for {
    oldState <- state.get
    _ = Console.err.println(s"${oldState}")
    _ <- state.update(s =>
      s.copy(lines = s.lines.updated(URI(params.textDocument.uri), linesOf(params.textDocument.text)))
    )
    newState <- state.get
    _ = Console.err.println(s"${newState}")
  } yield ()

  private def linesOf(text: String): Vector[String] =
    text.split("(\\n)|(\\r\\n)").toVector

}

case class Error(
  message: String,
  location: Range
)

sealed trait ParsingContext
object ParsingContext {
  case class SourceFile(
    startOffset: Int,
    stopOffset: Int
  )
}
