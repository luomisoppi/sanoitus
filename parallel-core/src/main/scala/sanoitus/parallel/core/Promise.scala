package sanoitus.parallel.core

import sanoitus._
import sanoitus.util._

import ParallelInterpreter._

trait Promise[+A] {
  def addWaiter(execution: Execution[_]): Option[Either[Throwable, A]]
  def flatMap[B](f: A => Promise[B]): Promise[B]
}

trait WritablePromise[A] extends Promise[A] {
  def setResult(value: Either[Throwable, A]): Unit
}

class UnitPromise[A](initial: Option[Either[Throwable, A]] = None) extends WritablePromise[A] {

  var result: Option[Either[Throwable, A]] = initial
  var waiters: List[Execution[_]] = List()

  override def addWaiter(execution: Execution[_]): Option[Either[Throwable, A]] =
    this.synchronized {
      result match {
        case None => {
          waiters = execution :: waiters
          None
        }
        case res @ Some(_) => res
      }
    }

  override def setResult(value: Either[Throwable, A]): Unit = {
    this.synchronized {
      if (result.isDefined)
        throw new IllegalStateException(
          s"Setting value to promise for another time - existing value $result, new one $value"
        )
      result = Some(value)
    }
    waiters.foreach(_.continueWith(value))
  }
  override def flatMap[B](f: A => Promise[B]): Promise[B] =
    FlatmapPromise(this, DependentQueue[PromiseKleisli, A, B](f))
}

case class FlatmapPromise[A, B](w: WritablePromise[A], kl: DependentQueue[PromiseKleisli, A, B]) extends Promise[B] {

  def awaitLoop[A1, B1](a: A1, queue: DependentDeque[PromiseKleisli, A1, B1]): Program[B1] =
    queue.take match {
      case Left(item) =>
        for {
          a <- Await(item.head(a))
          b <- awaitLoop(a, item.tail)
        } yield b
      case Right(f) => Await(f(a))
    }

  override def addWaiter(execution: Execution[_]): Option[Either[Throwable, B]] = {
    val prog =
      for {
        a <- Await(w)
        b <- awaitLoop(a, kl.dequeue)
        _ <- effect { _ =>
          Some(execution.continueWith(Right(b)))
        }
      } yield b

    def callback(result: Result[_]): Unit = result.value match {
      case Left(err) => execution.continueWith(Left(err))
      case _         => ()
    }
    execution.es.executeAsync(prog, callback)
    None
  }

  override def flatMap[C](g: B => Promise[C]): Promise[C] = FlatmapPromise(w, kl.enqueue(g))
}

object Promise {
  def apply[A](initial: Option[Either[Throwable, A]]): UnitPromise[A] = new UnitPromise(initial)
}
