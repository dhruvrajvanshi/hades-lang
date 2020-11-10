package hades.languageserver.logging

import cats.Applicative
import cats.effect.Sync
import cats.syntax.applicative._

trait LoggerF[F[_]] {
  def info(message: String): F[Unit]
  def warn(message: String): F[Unit]
  def debug(message: String): F[Unit]
}

object LoggerF {
  def make[F[_]: Applicative: Sync, C](clazz: Class[C]): F[LoggerF[F]] = {
    new LoggerFImpl[F, C](clazz).asInstanceOf[LoggerF[F]].pure[F]
  }
}

private class LoggerFImpl[F[_]: Applicative: Sync, C](val clazz: Class[C]) extends LoggerF[F] {
  override def info(message: String): F[Unit] = withTag("INFO", message)

  override def warn(message: String): F[Unit] = withTag("WARN", message)

  override def debug(message: String): F[Unit] = withTag("DEBUG", message)

  private def withTag(tag: String, message: String): F[Unit] = Sync[F].delay {
    System.err.println(s"${tag} (${clazz.getName}): $message")
  }

}
