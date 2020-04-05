package sanoitus

trait Execution[A] { self =>
  type Meta
  type Self[X] <: Execution[X]

  val es: ExecutionService
  val program: Program[A]
  val callback: ExecutionResult[A, Meta] => Unit
  val resources: Set[Resource[_]]
  val meta: Meta

  def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): Self[A]

  def continueWith(value: Either[Throwable, Any]): Unit
  def proceed(value: Any): Unit = continueWith(Right(value))
  def fail(err: Throwable): Unit = continueWith(Left(err))
}
