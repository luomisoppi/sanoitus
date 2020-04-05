package sanoitus.util

import sanoitus.Program

trait Monad[F[_]] extends Applicative[F] {

  implicit val self: Monad[F] = this

  def unit[A](a: => A): F[A]

  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A, G[_]](as: G[F[A]])(implicit ev: Traversable[G]): F[G[A]] =
    ev.sequence(as)(this)

  def whileM[A](md: F[A])(f: A => Boolean): F[A] =
    md.flatMap(a => if (f(a)) whileM(md)(f) else unit(a))

  def join[A](ff: F[F[A]]): F[A] = flatMap(ff)(identity)
}

object Monad {

  implicit val program: Monad[Program] = new Monad[Program] {
    override def unit[A](a: => A): Program[A] = sanoitus.unit(a)
    override def flatMap[A, B](a: Program[A])(f: A => Program[B]) = a.flatMap(f)
  }

  implicit val list = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f)
  }

  implicit val set = new Monad[Set] {
    override def unit[A](a: => A): Set[A] = Set(a)
    override def flatMap[A, B](a: Set[A])(f: A => Set[B]): Set[B] = a.flatMap(f)
  }

  implicit def state[S] = new Monad[({ type lambda[a] = State[S, a] })#lambda] {
    override def unit[A](a: => A) = (s: S) => (a, s)
    override def flatMap[A, B](a: ({ type lambda[a] = State[S, a] })#lambda[A])(f: A => ({
      type lambda[a] = State[S, a]
    })#lambda[B]) =
      (s: S) => {
        val (value, s2) = a(s)
        f(value)(s2)
      }
  }
}
