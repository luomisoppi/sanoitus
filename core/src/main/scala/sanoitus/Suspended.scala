package sanoitus

case class Suspended[A](private val exec: Execution[_]) {
  val es = exec.es
  def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): Suspended[A] = Suspended(exec.mapResources(f))
  def continueWith(value: Either[Throwable, A]): Unit = exec.continueWith(value)
  def proceed(value: A): Unit = continueWith(Right(value))
  def fail(err: Throwable): Unit = continueWith(Left(err))
}
