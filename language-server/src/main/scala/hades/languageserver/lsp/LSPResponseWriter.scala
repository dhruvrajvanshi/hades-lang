package hades.languageserver.lsp

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import hades.languageserver.logging.LoggerF
import hades.languageserver.writer.OutputStreamWriterF

trait LSPResponseWriter[F[_]] {
  def write(response: LSPResponse): F[Unit]
}

object LSPResponseWriter {
  def build[F[_]](
    inner: OutputStreamWriterF[F]
  )(implicit
    monad: Monad[F],
    sync: Sync[F],
  ): F[LSPResponseWriter[F]] = for {
    log <- LoggerF.build[F, LSPResponseWriterImpl[F]](classOf[LSPResponseWriterImpl[F]])
  } yield new LSPResponseWriterImpl[F](inner, log)
}

private class LSPResponseWriterImpl[F[_]](
  val inner: OutputStreamWriterF[F],
  val log: LoggerF[F]
)(implicit
  monad: Monad[F]
) extends LSPResponseWriter[F] {
  override def write(response: LSPResponse): F[Unit] =
    for {
      _ <- log.debug(s"Writing response $response")
      responseJson <- response.toJsonString.pure[F]
      _ <- inner.write(s"Content-Length: ${responseJson.length}\r\n\r\n")
      _ <- inner.write(responseJson)
      _ <- inner.flush()
    } yield ()
}
