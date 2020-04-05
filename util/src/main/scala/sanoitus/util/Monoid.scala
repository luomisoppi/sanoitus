package sanoitus.util

trait Monoid[A] extends Semigroup[A] {
  val zero: A
}
object Monoid {

  implicit val string = new Monoid[String] {
    override def op(x: String, y: String): String = x + y
    override val zero = ""
  }

  implicit def list[A] = new Monoid[List[A]] {
    override def op(x: List[A], y: List[A]): List[A] = x ++ y
    override val zero = List()
  }

  implicit def set[A] = new Monoid[Set[A]] {
    override def op(x: Set[A], y: Set[A]): Set[A] = x ++ y
    override val zero = Set()
  }

  implicit def map[K, V](implicit sg: Semigroup[V]) = new Monoid[Map[K, V]] {
    override def op(x: Map[K, V], y: Map[K, V]): Map[K, V] = y.foldLeft(x) { (acc, a) =>

      val (key, value) = a
      acc.get(key) match {
        case None           => acc + ((key, value))
        case Some(existing) => acc + ((key, sg.op(existing, value)))
      }
    }
    override val zero = Map()
  }
}
