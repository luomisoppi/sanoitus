package sanoitus.util

trait Semigroup[A] {
  def op(x: A, y: A): A
}

object Semigroup {

  implicit def list[A]: Semigroup[List[A]] = Monoid.list

  implicit def map[K, V](implicit sg: Semigroup[V]): Semigroup[Map[K, V]] = Monoid.map[K, V]
}
