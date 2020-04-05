package sanoitus.stream

import sanoitus._
import sanoitus.util._

trait StreamLanguage extends Language { self: Interpreter =>

  type Stream[+A]

  object Stream {
    def apply[A](a: A): Stream[A] = streamMonad.unit(a)
    def from[A](program: Program[A]): Stream[A] = fromProgram(program)
    def from[A](op: Op[A]): Stream[A] = fromOp(op)
    def empty[A]: Stream[A] = self.empty
    def from[A](iter: Iterable[A]): Stream[A] =
      iter.headOption match {
        case None    => empty
        case Some(h) => Stream(h) ++ from(iter.tail)
      }

    def fromOps[A](operations: Iterable[Op[A]]): Stream[A] =
      operations.headOption match {
        case None    => empty
        case Some(h) => from(h) ++ fromOps(operations.tail)
      }
  }

  def fromProgram[A](program: Program[A]): Stream[A]
  def fromOp[A](op: Op[A]): Stream[A]
  def empty[A]: Stream[A]

  implicit val streamMonad: Monad[Stream]

  abstract class StreamOps[A](stream: Stream[A]) extends MonadOps[Stream, A](stream)(streamMonad) {
    def filter(f: A => Boolean): Stream[A]

    def zip[B](b: Stream[B]): Stream[(A, B)]

    def ++[B >: A](rest: => Stream[B]): Stream[B]

    def take(count: Int): Stream[A]

    def drop(count: Int): Stream[A]

    def takeUntil(f: A => Boolean): Stream[A]

    def dropWhile(f: A => Boolean): Stream[A]

    def foldLeft[ACC](z: ACC)(f: (ACC, A) => ACC): Stream[ACC]

    def through[B](f: A => Program[B]): Stream[B]

    def repeat: Stream[A] = stream ++ stream.repeat

    def takeWhileDefined[B](implicit ev: A <:< Option[B]): Stream[B] =
      stream.map(ev(_)).flatMap {
        case None    => empty
        case Some(a) => Stream(a)
      }

    def takeWhileNotEmpty(implicit ev: A <:< Iterable[_]): Stream[A] =
      stream.takeUntil(ev(_).isEmpty).filter(!ev(_).isEmpty)

    def effect[B](f: A => B): Stream[B] = stream.through(a => sanoitus.effect { _ => Some(f(a)) })
  }

  implicit def ops[A](stream: Stream[A]): StreamOps[A]

  case class StreamData[A](value: A, next: Stream[A]) {
    override def toString = s"StreamData($value, ${next.getClass.getName})"
  }
  case class ReadStream[A](stream: Stream[A]) extends Operation[Option[StreamData[A]]] {
    override def toString = s"ReadStream(${stream.getClass.getName})"
  }

  def ReadAll[A](stream: Stream[A]): Program[List[A]] =
    ReadStream(stream.foldLeft(List[A]()) { (acc, a) => a :: acc }).map { _.get.value.reverse }

  def Process[A](stream: Stream[A]): Program[Unit] =
    ReadStream(stream).flatMap {
      case None       => unit(())
      case Some(data) => Process(data.next)
    }
}
