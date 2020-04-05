package sanoitus.parallel

import sanoitus.util._

package object core {

  implicit val promiseMon: Monad[Promise] = new Monad[Promise] {
    override def unit[A](a: => A): Promise[A] = Promise(Some(Right(a)))
    override def flatMap[A, B](a: Promise[A])(f: A => Promise[B]): Promise[B] = a.flatMap(f)
  }

  type PromiseKleisli[A, B] = Kleisli[Promise, A, B]
}
