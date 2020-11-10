package hades.languageserver.writer

import java.io.{BufferedWriter, OutputStream, OutputStreamWriter}

import cats.effect.Sync
import cats.implicits._

trait OutputStreamWriterF[F[_]] {
  def write(text: String): F[Unit]
}

object OutputStreamWriterF {
  def build[F[_]: Sync](stream: OutputStream): F[OutputStreamWriterF[F]] =
    new OutputStreamWriterFImpl[F](stream).asInstanceOf[OutputStreamWriterF[F]].pure[F]
}

private class OutputStreamWriterFImpl[F[_]: Sync](stream: OutputStream) extends OutputStreamWriterF[F] {
  val inner = new BufferedWriter(new OutputStreamWriter(stream))
  override def write(text: String): F[Unit] = Sync[F].delay(inner.write(text))
}
