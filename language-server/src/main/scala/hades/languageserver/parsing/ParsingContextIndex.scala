package hades.languageserver.parsing

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import hades.languageserver.logging.LoggerF
import hades.languageserver.lsp.{LSPRequestParams, Range}

trait ParsingContextIndex[F[_]] {
  def onDocumentOpen(params: LSPRequestParams.TextDocumentDidOpen): F[Unit]
  def onDocumentEdit(params: LSPRequestParams.TextDocumentDidChange): F[Unit]
}
object ParsingContextIndex {
  def build[F[_]: Sync: Monad]: F[ParsingContextIndex[F]] = for {
    stateRef <- Ref[F].of(State())
    log <- LoggerF.build[F, ParsingContextIndexImpl[F]](classOf)
  } yield new ParsingContextIndexImpl[F](stateRef, log)
}

case class URI(value: String) extends AnyVal
private case class State(
  lines: Map[URI, Vector[String]] = Map()
)
private class ParsingContextIndexImpl[F[_]: Sync: Monad](
  private val state: Ref[F, State],
  private val log: LoggerF[F]
) extends ParsingContextIndex[F] {

  override def onDocumentEdit(params: LSPRequestParams.TextDocumentDidChange): F[Unit] = {
    ().pure[F]
  }

  override def onDocumentOpen(params: LSPRequestParams.TextDocumentDidOpen): F[Unit] = log.profile("onDocumentOpen") {
    for {
      _ <- log.debug(s"textDocument/didOpen(${params.textDocument.uri}, ${params.textDocument.version})")
      oldState <- state.get
      _ <- log.debug(s"OldState: ${oldState.lines.size} URIs have lines in cache")
      _ <- state.update(s =>
        s.copy(lines = s.lines.updated(URI(params.textDocument.uri), linesOf(params.textDocument.text)))
      )
      newState <- state.get
      _ <- log.info(s"Lines cached for: ${newState.lines.size}")
      _ <- log.info(s"${newState.lines.keys.foldLeft("") { (acc, b) => s"$acc\n$b" }} ")
    } yield ()
  }

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
