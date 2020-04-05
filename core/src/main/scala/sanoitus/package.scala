package object sanoitus {
  type Op[+A] = Language#Operation[A]
  type FreeExec = Execution[A forSome { type A }]
  type Result[+A] = ExecutionResult[A, Execution[_]#Meta]

  implicit def liftOperationToProgram[A](op: Op[A]): Program[A] = Interpret(op)

  def unit[A](a: A): Program[A] = Return(a)

  def directive[A](thunk: (Execution[X] => Option[(Execution[X], Program[A])]) forSome { type X }): Program[A] =
    Directive(thunk)

  def effect[A](thunk: FreeExec => Option[A]): Program[A] = directive[A] { (exec: FreeExec) =>
    thunk(exec).map(res => (exec, Return(res)))
  }

  def fail[A](err: Throwable): Program[A] = effect { _ => throw err }

  def createResource[A](resourceValue: => A)(closer: A => Program[Unit],
                                             idx: Option[Int] = None): Program[Resource[A]] =
    directive { (exec: FreeExec) =>
      val resource =
        new Resource[A] {
          override val value = resourceValue
          override val index = idx
          override def close() = closer(resourceValue)
        }
      Some((exec.mapResources(_ + resource), Return(resource)))
    }

  def resource[A](resourceValue: => A)(closer: A => Unit): Program[Resource[A]] =
    createResource(resourceValue)(a => effect { _ => Some(closer(a)) })

  def resources[A](resourceValue: => A)(closer: A => Unit, closers: (A => Unit)*): Program[List[Resource[A]]] = {
    val progs = resource(resourceValue)(closer) +: closers.map(resource(resourceValue)(_))
    progs.foldLeft(unit(List[Resource[A]]())) { (acc, a) =>
      for {
        head <- acc
        next <- a
      } yield head :+ next
    }
  }

  def close(resource: Resource[_]): Program[Unit] = directive[Unit] { (exec: FreeExec) =>
    if (exec.resources.contains(resource)) {
      Some((exec.mapResources(_ - resource), resource.close()))
    } else {
      Some((exec, Return(())))
    }
  }
}
