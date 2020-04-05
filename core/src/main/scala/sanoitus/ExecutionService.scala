package sanoitus

import java.util.concurrent.Semaphore

trait ExecutionService { self =>

  type Exec[A] <: Execution[A]
  type Res[+A] = ExecutionResult[A, Exec[_]#Meta]

  protected val anomalies: AnomalySink

  def shutdown(): Unit

  def executeAsync[A](program: Program[A], callback: Res[A] => Unit): Unit

  def execute[A](program: Program[A]): Res[A] = {
    val sem = new Semaphore(0)
    var result: Res[A] = null
    def callback: Res[A] => Unit = res => { result = res; sem.release() }
    executeAsync(program, callback)
    sem.acquire()
    result
  }

  def executeUnsafe[A](program: Program[A]): A =
    execute(program).value match {
      case Right(res) => res
      case Left(t)    => throw t;
    }

  def continue[A](execution: Exec[A], value: Either[Throwable, Any]): Unit

  case class RightBoundFlatmap[A, +B](h: UnitProgram[A], f: A => Program[B]) extends Program[B]
  type FMP[A] = Flatmap[_, A]

  @annotation.tailrec
  final protected def bindRight[A](fmp: FMP[A]): Program[A] =
    fmp match {
      case fmp @ Flatmap(p: Flatmap[a, b], _) =>
        bindRight(Flatmap(p.h, (q: a) => Flatmap(p.f(q), fmp.f)))
      case fmp @ Flatmap(simple: UnitProgram[x], _) => RightBoundFlatmap(simple, fmp.f)
      case fmp @ Flatmap(n: RightBoundFlatmap[a, b], _) =>
        bindRight(Flatmap(n.h, (q: a) => Flatmap(n.f(q), fmp.f)))
      case err @ _ => throw new IllegalStateException("does not really happen: " + err)
    }

  @annotation.tailrec
  final def advance[A](program: Program[A], value: Any): Program[A] =
    program match {
      case x: Flatmap[x, A]        => advance(bindRight(x), value)
      case RightBoundFlatmap(_, f) => RightBoundFlatmap(Return(value), f)
      case _: UnitProgram[A]       => Return(value.asInstanceOf[A])
    }

  def runnableFor[A](execution: Exec[A]): Runnable =
    new Runnable() {
      override def run(): Unit =
        try {
          exec(execution) match {
            case None                      => ()
            case Some((result, execution)) => finalizeExecution(execution, Right(result))
          }
        } catch {
          case t: Throwable => finalizeExecution(execution, Left(t))
        }
    }

  def exec[A](execution: Exec[A]): Option[(A, Exec[A])]

  protected def finalizeExecution[A](
    execution: Exec[A],
    result: Either[Throwable, A]
  ): Unit = {

    result match {
      case Left(err) => anomalies.error(execution, err)
      case _         => ()
    }

    execution.resources.foreach { resource =>
      try {
        val program =
          for {
            _ <- directive[Unit] { (exec: FreeExec) => Some((exec.mapResources(_ + resource), Return(()))) }
            _ <- close(resource)
          } yield ()

        executeUnsafe(program)
      } catch {
        case t: Throwable => anomalies.error(execution, new AutoCloseFailed(t))
      }
    }

    execution.callback(new ExecutionResult[A, execution.Meta] {
      override val value = result
      override val meta = execution.meta
    })
  }
}
