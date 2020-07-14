package sanoitus

case class BasicExecution[A](
  val program: Program[A],
  override val callback: ExecutionResult[A, Unit] => Unit,
  override val resources: Set[Resource[_]],
  override val es: BasicExecutionService
) extends Execution[A] {
  override type Meta = Unit
  override type Self = BasicExecution[A]
  override val meta = ()
  override def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): BasicExecution[A] =
    BasicExecution(program, callback, f(resources), es)
  override def continueWith(value: Either[Throwable, Any]): Unit = es.continue(this, value)
  def setProgram(prog: Program[A]) =
    BasicExecution(prog, callback, resources, es)

}
