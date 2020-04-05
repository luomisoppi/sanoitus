package sanoitus

case class BasicExecution[A](
  override val program: Program[A],
  override val callback: ExecutionResult[A, Unit] => Unit,
  override val resources: Set[Resource[_]],
  override val es: BasicExecutionService
) extends Execution[A] {
  override type Meta = Unit
  override type Self[X] = BasicExecution[X]
  override val meta = ()
  override def continueWith(value: Either[Throwable, Any]): Unit = es.continue(this, value)
  override def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): BasicExecution[A] =
    BasicExecution(program, callback, f(resources), es)
  def setProgram(prog: Program[A]) =
    BasicExecution(prog, callback, resources, es)
}
