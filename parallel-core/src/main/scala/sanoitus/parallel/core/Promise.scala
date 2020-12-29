package sanoitus.parallel.core

import sanoitus._
import sanoitus.util._

import ParallelInterpreter._

trait Promise[+A] {
  def addWaiter(s: Suspended[_ >: A]): Option[Either[Throwable, A]]
  def flatMap[B](f: A => Promise[B]): Promise[B]
}

trait WritablePromise[A] extends Promise[A] {
  def setResult(value: Either[Throwable, A]): Unit
}

class UnitPromise[A](initial: Option[Either[Throwable, A]] = None) extends WritablePromise[A] {

  var result: Option[Either[Throwable, A]] = initial
  var waiters: List[Suspended[_ >: A]] = List()

  override def addWaiter(s: Suspended[_ >: A]): Option[Either[Throwable, A]] =
    this.synchronized {
      result match {
        case None => {
          waiters = s :: waiters
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
      case Left(item) => Await(item.head(a)).flatMap(a => awaitLoop(a, item.tail))
      case Right(f)   => Await(f(a))
    }

  override def addWaiter(suspended: Suspended[_ >: B]): Option[Either[Throwable, B]] = {
    val prog =
      for {
        a <- Await(w)
        b <- awaitLoop(a, kl.dequeue)
        _ <- effect[Unit] { _ => Some(suspended.proceed(b)) }
      } yield ()

    suspended.es.executeAsyncPlain(prog) {
      case Left(err) => suspended.continueWith(Left(err))
      case _         => ()
    }
    None
  }

  override def flatMap[C](g: B => Promise[C]): Promise[C] = FlatmapPromise(w, kl.enqueue(g))
}

object Promise {
  def apply[A](initial: Option[Either[Throwable, A]]): UnitPromise[A] = new UnitPromise(initial)
}
