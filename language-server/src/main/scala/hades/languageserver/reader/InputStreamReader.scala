package hades.languageserver.reader

import java.io.{BufferedReader, InputStream, InputStreamReader}

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._

trait InputStreamReaderF[F[_]] {
  def readLine(): F[String]
  def readString(length: Int): F[String]
}

object InputStreamReaderF {
  def build[F[_]: Sync: Monad](inputStream: InputStream): F[InputStreamReaderF[F]] = {
    Sync[F].delay(new BufferedReader(new InputStreamReader(inputStream))).map(new InputStreamReaderFImpl[F](_))
  }
}

private class InputStreamReaderFImpl[F[_]: Sync: Monad](val inner: BufferedReader) extends InputStreamReaderF[F] {

  override def readLine(): F[String] = Sync[F].delay(inner.readLine())

  override def readString(length: Int): F[String] = for {
    buffer <- Sync[F].delay(new Array[Char](length))
    _ <- Sync[F].delay(inner.read(buffer, 0, length))
    text <- Sync[F].delay(new String(buffer))
  } yield text

}
