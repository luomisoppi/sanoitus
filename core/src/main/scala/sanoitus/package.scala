package object sanoitus {
  type Op[+A] = Language#Operation[A]

  def unit[A](a: A): Program[A] = Return(a)

  implicit def liftOperationToProgram[A](op: Op[A]): Program[A] = Interpret(op)

  case object Common extends Interpreter {
    sealed trait Op[+A] extends Operation[A]

    case class Unit[A](value: A) extends Op[A]

    def apply[A](op: Op[A]): Program[A] =
      op match {
        case Unit(value) => unit(value)
      }

    def close() = ()
  }

  def Unit[A](a: A) = Common.Unit(a)

  def effect[A](f: Suspended[A] => Option[A]): Program[A] = Effect(f)

  def fail[A](err: Throwable): Program[A] = effect[A] { _ => throw err }

  val peekResources = PeekResources()

  def mapResources(f: Set[Resource[_]] => Set[Resource[_]]): Program[Unit] =
    MapResources(f)

  def createResource[A](resourceValue: => A)(closer: A => Program[Unit],
                                             idx: Option[Int] = None): Program[Resource[A]] =
    for {
      res <- effect[Resource[A]] { _ =>
        Some(new Resource[A] {
          override val value = resourceValue
          override val index = idx
          override def close() = closer(resourceValue)
        })
      }
      _ <- mapResources(_ + res)
    } yield res

  def resource[A](resourceValue: => A)(closer: A => Unit): Program[Resource[A]] =
    createResource(resourceValue)(a => effect[Unit] { _ => Some(closer(a)) })

  def resources[A](resourceValue: => A)(closer: A => Unit, closers: (A => Unit)*): Program[List[Resource[A]]] = {
    val progs = resource(resourceValue)(closer) +: closers.map(resource(resourceValue)(_))
    progs.foldLeft(unit(List[Resource[A]]())) { (acc, a) =>
      for {
        head <- acc
        next <- a
      } yield head :+ next
    }
  }

  def close(resource: Resource[_]): Program[Unit] =
    for {
      resources <- peekResources
      _ <- {
        if (resources.contains(resource))
          for {
            _ <- mapResources(_ - resource)
            _ <- resource.close()
          } yield ()
        else
          unit(())
      }
    } yield ()

}
