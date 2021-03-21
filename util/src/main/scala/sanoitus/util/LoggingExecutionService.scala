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

  override def exec[A](initial: Exec[A]): Option[(Either[Throwable, A], LoggingExecution[A])] = {
    var execution = initial
    var result: Option[A] = null
    var error: Option[Throwable] = None
    try {
      while (result == null) {

        execution.program match {
          case p: Flatmap[_, _] => execution = execution.setProgram(bindRight(p))

          case RightBoundFlatmap(MapResources(f), g) => {
            val post = execution.mapResources(f)
            execution = post.setProgram(g(())).addMapResources(execution.resources, post.resources)
          }

          case RightBoundFlatmap(PeekResources(), g) =>
            execution = execution.setProgram(RightBoundFlatmap(Return(execution.resources), g))

          case RightBoundFlatmap(Return(value, false), g: Function1[Any, Program[A]] @unchecked) =>
            execution = execution
              .setProgram(g(value))
              .addOp(InternalInterpreter.Return(value))
              .addReturn(value)

          case RightBoundFlatmap(Return(value, true), g: Function1[Any, Program[A]] @unchecked) =>
            execution = execution.setProgram(g(value))

          case RightBoundFlatmap(Interpret(op), g) => {
            val interpreted =
              for {
                _ <- IncDepth(op)
                res <- op.interpret
                _ <- DecDepth(res)
              } yield res

            execution = execution.setProgram(Flatmap(interpreted, g))
          }

          case RightBoundFlatmap(IncDepth(op), g) =>
            execution = execution.setProgram(g(())).addOp(op).incDepth

          case RightBoundFlatmap(DecDepth(res), g) =>
            execution = execution.setProgram(g(())).decDepth.addReturn(res)

          case p @ RightBoundFlatmap(Effect(f), g) => {
            f(Suspended(execution.setProgram(p).addEffect())) match {
              case Some(value) => execution = execution.setProgram(RightBoundFlatmap(Return(value), g)).addEffect()
              case None        => result = None
            }
          }

          case Interpret(op) => {
            val interpreted =
              for {
                _ <- IncDepth(op)
                res <- op.interpret
                _ <- DecDepth(res)
              } yield res

            execution = execution.setProgram(interpreted)
          }

          case MapResources(f) => {
            val post = execution.mapResources(f)
            result = Some(())
            execution = post.addMapResources(execution.resources, post.resources)
          }

          case PeekResources() => result = Some(execution.resources)

          case p @ Effect(f) => {
            f(Suspended(execution.setProgram(p).addEffect())) match {
              case Some(value) => result = Some(value)
              case None        => result = None
            }
          }

          case Return(value, false) => {
            execution = execution.addOp(InternalInterpreter.Return(value)).addReturn(value)
            result = Some(value)
          }

          case Return(value, true) => {
            execution = execution.addMap(value)
            result = Some(value)
          }

          case IncDepth(op) => execution = execution.addOp(op).incDepth

          case DecDepth(res) => execution = execution.decDepth.addReturn(res)

          case err @ _ => throw new IllegalStateException(s"Execution error: $err")
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
    )

    new LoggingExecutionService(tp, anomalies)
  }
}
