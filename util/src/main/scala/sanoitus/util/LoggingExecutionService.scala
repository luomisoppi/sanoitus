package sanoitus.util

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

import sanoitus._

class LoggingExecutionService(
  val threadPool: ThreadPoolExecutor,
  override val anomalies: AnomalySink
) extends ExecutionService { self =>

  override type Exec[A] = LoggingExecution[A]

  def executeAsync[A](program: Program[A], callback: Res[A] => Unit) =
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
    case class Return[A](value: A) extends Operation[A]
    def apply[A](op: Operation[A]): Program[A] = ???
    def close(): Unit = ???
  }

  @annotation.tailrec
  final override def exec[A](execution: LoggingExecution[A]): Option[(A, LoggingExecution[A])] =
    execution.program match {
      case p: Flatmap[_, _] => exec(execution.setProgram(bindRight(p)))

      case RightBoundFlatmap(directive: Directive[_, A, Exec] @unchecked, g) => {
        directive.f(execution.addDirective()) match {
          case Some((e, prog)) => exec(e.setProgram(Flatmap(prog, g)))
          case None            => None
        }
      }

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

      case Interpret(op) => {
        val interpreted =
          for {
            _ <- IncDepth(op)
            res <- op.interpret
            _ <- DecDepth(res)
          } yield res

        exec(execution.setProgram(interpreted))
      }

      case directive: Directive[A, A, Exec] @unchecked =>
        directive.f(execution.addDirective()) match {
          case Some((e, prog)) => exec(e.setProgram(prog))
          case None            => None
        }

      case Return(value, false) =>
        Some((value, execution.addOp(InternalInterpreter.Return(value)).addReturn(value)))

      case Return(value, true) =>
        Some((value, execution.addMap(value)))

      case IncDepth(op) => exec(execution.addOp(op).incDepth)

      case DecDepth(res) => exec(execution.decDepth.addReturn(res))
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
