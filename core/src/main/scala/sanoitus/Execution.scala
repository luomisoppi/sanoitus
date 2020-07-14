package sanoitus

trait Execution[A] { self =>
  type Meta
  type Self >: self.type <: Execution[A]

  val es: ExecutionService
  val callback: ExecutionResult[A, Meta] => Unit
  val resources: Set[Resource[_]]
  val meta: Meta

  def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): Self
  def continueWith(value: Either[Throwable, Any]): Unit
}
