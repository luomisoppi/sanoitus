package sanoitus.parallel.core

import sanoitus._
import sanoitus.parallel._

object ParallelInterpreter extends ParallelLanguage with Interpreter {

  override type Promise[+A] = sanoitus.parallel.core.Promise[A]
  override type WritablePromise[A] = sanoitus.parallel.core.WritablePromise[A]
  override val promiseMonad = promiseMon

  def apply[A](op: Op[A]): Program[A] =
    op match {
      case fork: Fork[A @unchecked] =>
        for {
          resources <- peekResources
          promise <- effect[Promise[A]] { s =>
            if (fork.resources.toSet.subsetOf(resources)) {
              val p = Promise(None: Option[Either[Throwable, A]])

              val newProg =
                for {
                  _ <- mapResources { _ ++ fork.resources.toSet }
                  x <- fork.program
                } yield x

              s.es.executeAsyncPlain(newProg)(p.setResult(_))
              Some(p)
            } else {
              throw IllegalFork
            }
          }
          _ <- mapResources(_ -- fork.resources.toSet)
        } yield promise

      case await: Await[A @unchecked] => {
        effect[A] { suspended =>
          await.promise.addWaiter(suspended).map {
            case Left(err)    => throw err
            case Right(value) => value
          }
        }
      }

      case _: CreatePromise[a] => {
        unit(Promise(None: Option[Either[Throwable, a]]))
      }

      case FulfillPromise(promise, value) =>
        effect[Unit] { _ =>
          promise.setResult(value)
          Some(())
        }
    }

  def close(): Unit = {}
}
