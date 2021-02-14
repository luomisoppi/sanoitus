package sanoitus

case class Suspended[A](private val exec: Execution[_]) {
  val es = exec.es
  def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): Suspended[A] = Suspended(exec.mapResources(f))
  def continueWith(value: Either[Throwable, A]): Unit = exec.continueWith(value)
  def proceed(value: A): Unit = continueWith(Right(value))
  def fail(err: Throwable): Unit = continueWith(Left(err))

  def map[B](f: B => A): Suspended[B] = new Suspended[B](exec) {
    override def continueWith(value: Either[Throwable, B]): Unit = exec.continueWith(value.map(f))
  }
}
