package sanoitus.util

trait Traversable[F[_]] extends Functor[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
}

object Traversable {

  implicit val list = new Traversable[List] {
    def map[A, B](a: List[A])(f: A => B): List[B] = a.map(f)
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit ev: Applicative[G]): G[List[B]] =
      fa.foldRight(ev.unit(List[B]()))((a, acc) => ev.map2(f(a), acc)(_ :: _))
  }

  implicit val option = new Traversable[Option] {
    def map[A, B](a: Option[A])(f: A => B): Option[B] = a.map(f)
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Option[B]] =
      fa.map(f) match {
        case Some(g) => ev.map(g)(Some(_))
        case None    => ev.unit(None)
      }
  }
}
