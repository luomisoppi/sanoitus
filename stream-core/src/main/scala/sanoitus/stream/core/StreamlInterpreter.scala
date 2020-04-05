package sanoitus.stream.core

import scala.collection.immutable.Queue

import sanoitus._
import sanoitus.stream._
import sanoitus.util._

object StreamInterpreter extends Interpreter with StreamLanguage {

  trait Stream[+A]

  case class EmptyStream[+A]() extends Stream[A]

  case class UnitStream[+A](program: Program[A]) extends Stream[A]

  trait ConcatStream[+A] extends Stream[A] {
    val head: Stream[A]
    def tail: Stream[A]

    override def toString = s"ConcatStream($head, $tail)"
  }
  object ConcatStream {
    def apply[A](_head: Stream[A], _tail: => Stream[A]) = new ConcatStream[A] {
      override val head = _head
      override def tail = _tail
    }

    def unapply[A](c: ConcatStream[A]): Option[(Stream[A], Stream[A])] = Some((c.head, c.tail))
  }

  case class FoldStream[A, ACC](stream: Stream[A], zero: ACC, f: (ACC, A) => ACC) extends Stream[ACC]

  case class FlatmapStream[A, +B](stream: Stream[A], f: A => Stream[B]) extends Stream[B]

  case class FilterStream[A](stream: Stream[A], f: A => Boolean) extends Stream[A]

  case class TakeStream[A](stream: Stream[A], count: Int) extends Stream[A]

  case class TakeUntilStream[A](stream: Stream[A], f: A => Boolean) extends Stream[A]

  case class DropStream[A](stream: Stream[A], count: Int) extends Stream[A]

  case class DropWhileStream[A](stream: Stream[A], f: A => Boolean) extends Stream[A]

  case class DropRightStream[A](stream: Stream[A], count: Int, buffer: Queue[A]) extends Stream[A]

  case class ThroughStream[A, +B](stream: Stream[A], f: A => Program[B]) extends Stream[B]

  case class ZipStream[+A, +B](a: Stream[A], b: Stream[B]) extends Stream[(A, B)]

  override def fromProgram[A](program: Program[A]) = UnitStream(program)
  override def fromOp[A](op: Op[A]) = UnitStream(op)
  override def empty[A] = EmptyStream()

  implicit override val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = UnitStream(Return(a))
    override def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] = FlatmapStream(a, f)
  }

  implicit override def ops[A](stream: Stream[A]): StreamOps[A] = new StreamOps[A](stream) {

    def filter(f: A => Boolean) = FilterStream(stream, f)

    def zip[B](b: Stream[B]): Stream[(A, B)] = ZipStream(stream, b)

    def ++[B >: A](rest: => Stream[B]): Stream[B] = ConcatStream(stream, rest)

    def foldLeft[ACC](z: ACC)(f: (ACC, A) => ACC): Stream[ACC] = FoldStream(stream, z, f)

    def take(count: Int): Stream[A] = TakeStream(stream, count)

    def takeUntil(f: A => Boolean): Stream[A] = TakeUntilStream(stream, f)

    def drop(count: Int): Stream[A] = DropStream(stream, count)

    def dropWhile(f: A => Boolean): Stream[A] = DropWhileStream(stream, f)

    def through[B](f: A => Program[B]): Stream[B] = ThroughStream(stream, f)
  }

  def apply[A](op: Operation[A]): Program[A] =
    op match {
      case rs: ReadStream[x] => {
        rs.stream match {
          case EmptyStream() => unit(None)

          case UnitStream(prg) => prg.map(value => Some(StreamData(value, EmptyStream())))

          case ConcatStream(head, tail) =>
            ReadStream(head).flatMap {
              case None                                   => ReadStream(tail)
              case Some(StreamData(value, EmptyStream())) => unit(Some(StreamData(value, tail)))
              case Some(StreamData(value, next))          => unit(Some(StreamData(value, ConcatStream(next, tail))))
            }

          case FilterStream(stream, f) =>
            ReadStream(stream).flatMap {
              case None                                      => unit(None)
              case Some(StreamData(value, next)) if f(value) => unit(Some(StreamData(value, FilterStream(next, f))))
              case Some(StreamData(_, next))                 => ReadStream(next.filter(f))
            }

          case TakeUntilStream(stream, f) =>
            ReadStream(stream).flatMap {
              case None                                     => unit(None)
              case Some(StreamData(value, _)) if (f(value)) => unit(Some(StreamData(value, EmptyStream())))
              case Some(StreamData(value, next))            => unit(Some(StreamData(value, TakeUntilStream(next, f))))
            }

          case FoldStream(stream, zero, f) =>
            ReadStream(stream).flatMap {
              case None                          => unit(Some(StreamData(zero, EmptyStream[x])))
              case Some(StreamData(value, next)) => ReadStream(FoldStream(next, f(zero, value), f))
            }

          case FlatmapStream(stream, f) =>
            ReadStream(stream).flatMap {
              case None                          => unit(None)
              case Some(StreamData(value, next)) => ReadStream(ConcatStream(f(value), FlatmapStream(next, f)))
            }

          case TakeStream(stream, count) =>
            if (count <= 0) {
              unit(None)
            } else {
              ReadStream(stream).flatMap {
                case None                          => unit(None)
                case Some(StreamData(value, next)) => unit(Some(StreamData(value, TakeStream(next, count - 1))))
              }
            }

          case DropStream(stream, count) =>
            if (count <= 0) {
              ReadStream(stream)
            } else {
              ReadStream(stream).flatMap {
                case None                      => unit(None)
                case Some(StreamData(_, next)) => ReadStream(DropStream(next, count - 1))
              }
            }

          case DropWhileStream(stream, f) =>
            ReadStream(stream).flatMap {
              case Some(StreamData(value, next)) if f(value) => ReadStream(DropWhileStream(next, f))
              case value                                     => unit(value)
            }

          case DropRightStream(stream, count, buffer) =>
            if (count <= 0) {
              ReadStream(stream)
            } else if (buffer.length < count + 1) {
              ReadStream(stream).flatMap {
                case None                          => unit(None)
                case Some(StreamData(value, next)) => ReadStream(DropRightStream(next, count, buffer.enqueue(value)))
              }
            } else {
              val (value, tail) = buffer.dequeue
              unit(Some(StreamData(value, DropRightStream(stream, count, tail))))
            }

          case ThroughStream(stream, f) =>
            ReadStream(stream).flatMap {
              case None                          => unit(None)
              case Some(StreamData(value, next)) => f(value).map(x => Some(StreamData(x, ThroughStream(next, f))))
            }

          case ZipStream(streama, streamb) =>
            for {
              a <- ReadStream(streama)
              b <- ReadStream(streamb)
            } yield (a, b) match {
              case (None, _) => None
              case (_, None) => None
              case (Some(StreamData(a, nexta)), Some(StreamData(b, nextb))) =>
                Some(StreamData((a, b), ZipStream(nexta, nextb)))
            }
        }
      }
    }

  def close(): Unit = ()
}
