package sanoitus

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

class BasicExecutionService(val threadPool: ThreadPoolExecutor, val anomalies: AnomalySink) extends ExecutionService {
  self =>

  override type Meta = Unit
  override type Exec[A] = BasicExecution[A]

  def executeAsync[A](program: Program[A])(callback: Res[A] => Unit) =
    threadPool.execute(runnableFor(BasicExecution(program, callback, Set(), self)))

  def continue[A](execution: BasicExecution[A], value: Either[Throwable, Any]): Unit =
    value match {
      case Right(v)  => threadPool.execute(runnableFor(execution.setProgram(advance(execution.program, v))))
      case Left(err) => finalizeExecution(execution, Left(err))
    }

  @annotation.tailrec
  final def execP[A](program: Program[A]): Program[A] =
    program match {
      case p: Flatmap[_, A]                       => execP(bindRight(p))
      case RightBoundFlatmap(Return(value, _), g) => execP(g(value))
      case RightBoundFlatmap(Interpret(op), g)    => execP(Flatmap(op.interpret, g))
      case Interpret(op)                          => execP(op.interpret)
      case _                                      => program
    }

  override def exec[A](initial: Exec[A]): Option[(Either[Throwable, A], Exec[A])] = {
    var execution = initial
    var result: Option[A] = null
    var error: Option[Throwable] = None
    try {
      while (result == null) {
        execP(execution.program) match {
          case p @ RightBoundFlatmap(Effect(f), g) => {
            f(Suspended(execution.setProgram(p))) match {
              case Some(value) => execution = execution.setProgram(g(value))
              case None        => result = None
            }
          }

          case RightBoundFlatmap(MapResources(f), g) => execution = execution.mapResources(f).setProgram(g(()))

          case RightBoundFlatmap(PeekResources(), g) => execution = execution.setProgram(g(execution.resources))

          case Return(value, _) => result = Some(value)

          case PeekResources() => result = Some(execution.resources)

          case MapResources(f) => {
            execution = execution.mapResources(f)
            result = Some(())
          }

          case p @ Effect(f) => {
            f(Suspended(execution.setProgram(p))) match {
              case Some(value) => result = Some(value)
              case None        => result = None
            }
          }

          case err @ _ => throw new IllegalStateException(s"Program execution error: $err")
        }
      }
    } catch {
      case t: Throwable => error = Some(t)
    }

    (result, error) match {
      case (_, Some(err)) => Some((Left(err), execution))
      case (Some(res), _) => Some((Right(res), execution))
      case (None, _)      => None
    }
  }

  override def shutdown(): Unit = threadPool.shutdown()
}

object BasicExecutionService {
  def apply(
    poolSize: Int = 16,
    queueSize: Int = 1000,
    anomalies: AnomalySink = StdErrAnomalySink
  ): BasicExecutionService = {
    val tp = new ThreadPoolExecutor(
      poolSize,
      queueSize,
      0,
      TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[java.lang.Runnable](queueSize),
      new ThreadPoolExecutor.CallerRunsPolicy()
    );

    new BasicExecutionService(tp, anomalies)
  }
}
