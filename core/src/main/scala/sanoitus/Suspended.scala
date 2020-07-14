package sanoitus

case class Suspended[A](exec: Execution[_]) {
  val es = exec.es
  def continueWith(value: Either[Throwable, A]): Unit = exec.continueWith(value)
  def proceed(value: A): Unit = continueWith(Right(value))
  def fail(err: Throwable): Unit = continueWith(Left(err))
}
