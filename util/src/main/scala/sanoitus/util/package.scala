package sanoitus

import java.io.StringWriter
import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicReference

package object util {

  def throwableToString(t: Throwable): String =
    if (t != null) {
      val sw = new StringWriter();
      val pw = new PrintWriter(sw);
      t.printStackTrace(pw);
      sw.toString
    } else {
      "no exception"
    }

  def atom[V](v: V): AtomicReference[V] = new AtomicReference(v)

  def cas[V](ref: AtomicReference[V])(next: V => V): V = {
    while (true) {
      val expect = ref.get
      val updated = next(expect)
      if (ref.compareAndSet(expect, updated)) return updated
    }
    throw new AssertionError
  }

  def casF[V, R](ref: AtomicReference[V])(next: V => (R, V)): R = {
    while (true) {
      val expect = ref.get
      val (returned, updated) = next(expect)
      if (ref.compareAndSet(expect, updated)) return returned
    }
    throw new AssertionError
  }

  type Kleisli[F[_], A, B] = A => F[B]
  type State[S, A] = S => (A, S)

  implicit class SemigroupOps[A](value: A)(implicit sg: Semigroup[A]) {
    def |+|(another: A): A = sg.op(value, another)
  }

  implicit class MonoidOps[A](value: A)(implicit sg: Monoid[A]) {
    def |+|(another: A): A = sg.op(value, another)
  }

  implicit class SequenceOps[F[_], G[_], A](a: F[G[A]])(implicit trav: Traversable[F], app: Applicative[G]) {
    def sequence = trav.sequence(a)
  }

  implicit class MonadOps[F[_], A](a: F[A])(implicit mon: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = mon.flatMap(a)(f)
    def map[B](f: A => B): F[B] = mon.map(a)(f)
  }

  implicit class JoinOps[F[_], A](a: F[F[A]])(implicit mon: Monad[F]) {
    def join = a.flatMap(identity)
  }

}
