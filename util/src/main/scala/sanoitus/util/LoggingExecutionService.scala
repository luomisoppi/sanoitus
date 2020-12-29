package sanoitus.util

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

import sanoitus._

class LoggingExecutionService(
  val threadPool: ThreadPoolExecutor,
  override val anomalies: AnomalySink
) extends ExecutionService { self =>

  override type Meta = Log
  override type Exec[A] = LoggingExecution[A]

  def executeAsync[A](program: Program[A])(callback: Res[A] => Unit) =
    threadPool.execute(
      runnableFor(
        LoggingExecution(program, callback, Set(), self)
      )
    )

  def continue[A](execution: LoggingExecution[A], value: Either[Throwable, Any]): Unit =
    value match {
      case Right(value) => threadPool.execute(runnableFor(execution.setProgram(advance(execution.program, value))))
      case Left(err)    => finalizeExecution(execution, Left(err))
    }

  private case class IncDepth(op: Op[_]) extends UnitProgram[Unit]
  private case class DecDepth[A](res: A) extends UnitProgram[Unit]

  case object InternalInterpreter extends Interpreter {
    sealed trait Op[+A] extends Operation[A]
    case class Return[A](value: A) extends Op[A]
    def apply[A](op: Op[A]): Program[A] = ???
    def close(): Unit = ???
  }

  @annotation.tailrec
  final override def exec[A](execution: LoggingExecution[A]): Option[(A, LoggingExecution[A])] =
    execution.program match {
      case p: Flatmap[_, _] => exec(execution.setProgram(bindRight(p)))

      case RightBoundFlatmap(MapResources(f), g) => {
        val post = execution.mapResources(f)
        exec(post.setProgram(g(())).addMapResources(execution.resources, post.resources))
      }

      case RightBoundFlatmap(PeekResources(), g) =>
        exec(execution.setProgram(RightBoundFlatmap(Return(execution.resources), g)))

      case RightBoundFlatmap(Return(value, false), g: Function1[Any, Program[A]] @unchecked) =>
        exec(
          execution
            .setProgram(g(value))
            .addOp(InternalInterpreter.Return(value))
            .addReturn(value)
        )

      case RightBoundFlatmap(Return(value, true), g: Function1[Any, Program[A]] @unchecked) =>
        exec(execution.setProgram(g(value)))

      case RightBoundFlatmap(Interpret(op), g) => {
        val interpreted =
          for {
            _ <- IncDepth(op)
            res <- op.interpret
            _ <- DecDepth(res)
          } yield res

        exec(execution.setProgram(Flatmap(interpreted, g)))
      }

      case RightBoundFlatmap(IncDepth(op), g) =>
        exec(execution.setProgram(g(())).addOp(op).incDepth)

      case RightBoundFlatmap(DecDepth(res), g) =>
        exec(execution.setProgram(g(())).decDepth.addReturn(res))

      case p @ RightBoundFlatmap(Effect(f), g) => {
        f(Suspended(execution.setProgram(p).addEffect())) match {
          case Some(value) => exec(execution.setProgram(RightBoundFlatmap(Return(value), g)).addEffect())
          case None        => None
        }
      }

      case Interpret(op) => {
        val interpreted =
          for {
            _ <- IncDepth(op)
            res <- op.interpret
            _ <- DecDepth(res)
          } yield res

        exec(execution.setProgram(interpreted))
      }

      case MapResources(f) => {
        val post = execution.mapResources(f)
        Some(((), post.addMapResources(execution.resources, post.resources)))
      }

      case PeekResources() => Some((execution.resources, execution))

      case p @ Effect(f) => {
        f(Suspended(execution.setProgram(p).addEffect())) match {
          case Some(value) => Some((value, execution))
          case None        => None
        }
      }

      case Return(value, false) =>
        Some((value, execution.addOp(InternalInterpreter.Return(value)).addReturn(value)))

      case Return(value, true) =>
        Some((value, execution.addMap(value)))

      case IncDepth(op) => exec(execution.addOp(op).incDepth)

      case DecDepth(res) => exec(execution.decDepth.addReturn(res))

      case err @ _ => throw new IllegalStateException(s"Execution error: $err")
    }

  override def shutdown(): Unit = threadPool.shutdown()
}

object LoggingExecutionService {
  def apply(
    poolSize: Int = 16,
    queueSize: Int = 1000,
    anomalies: AnomalySink = StdErrAnomalySink
  ): LoggingExecutionService = {
    val tp = new ThreadPoolExecutor(
      poolSize,
      queueSize,
      0,
      TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[java.lang.Runnable](queueSize),
      new ThreadPoolExecutor.CallerRunsPolicy()
    );

    new LoggingExecutionService(tp, anomalies)
  }
}
