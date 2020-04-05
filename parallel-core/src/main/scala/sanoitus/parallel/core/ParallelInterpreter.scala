package sanoitus.parallel.core

import sanoitus._
import sanoitus.parallel._

object ParallelInterpreter extends Interpreter with ParallelLanguage {

  override type Promise[+A] = sanoitus.parallel.core.Promise[A]
  override type WritablePromise[A] = sanoitus.parallel.core.WritablePromise[A]
  override val promiseMonad = promiseMon

  def apply[A](op: Operation[A]): Program[A] =
    op match {
      case fork: Fork[A @unchecked] =>
        for {
          promise <- directive[Promise[A]] { (execution: FreeExec) =>
            val ok = fork.resources.foldLeft(true) { (acc, resource) => acc && execution.resources.contains(resource) }
            if (!ok) throw IllegalFork

            val p = Promise(None: Option[Either[Throwable, A]])

            val newProg =
              for {
                _ <- directive[Unit] { (e: FreeExec) => Some((e.mapResources(_ ++ fork.resources.toSet), unit(()))) }
                x <- fork.program
              } yield x

            execution.es.executeAsync(newProg, (x: Result[A]) => p.setResult(x.value))
            Some((execution.mapResources(_ -- fork.resources.toSet), unit(p)))
          }
        } yield promise

      case Await(promise) => {
        effect { execution =>
          promise.addWaiter(execution).map {
            case Left(err)    => throw err
            case Right(value) => value
          }
        }
      }

      case _: CreatePromise[a] => {
        unit(Promise(None: Option[Either[Throwable, a]]))
      }

      case FulfillPromise(promise, value) =>
        effect { _ =>
          promise.setResult(value)
          Some(())
        }
    }

  def close(): Unit = {}
}
