package hades.languageserver.parsing

trait ParsingContextIndex[F[_]] {
  def onDocumentOpen: F[Unit]
}

class ParsingContextIndexImpl[F[_]] extends ParsingContextIndex[F] {
  override def onDocumentOpen: F[Unit] = ???
}
