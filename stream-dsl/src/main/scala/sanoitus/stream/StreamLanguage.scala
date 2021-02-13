package sanoitus.stream

import sanoitus._
import sanoitus.util._
import java.nio.channels.AsynchronousByteChannel
import java.nio.ByteBuffer
import java.nio.channels.CompletionHandler
import java.nio.channels.AsynchronousFileChannel

trait StreamLanguage extends Language { self: Interpreter =>
  sealed trait Op[+A] extends Operation[A]

  type Stream[+A]

  object Stream {
    def apply[A](a: A): Stream[A] = streamMonad.unit(a)
    def from[A](program: Program[A]): Stream[A] = fromProgram(program)
    def from[A](op: Language#Operation[A]): Stream[A] = fromOp(op)
    def empty[A]: Stream[A] = self.empty
    def from[A](iter: Iterable[A]): Stream[A] =
      iter.headOption match {
        case None    => empty
        case Some(h) => Stream(h) ++ from(iter.tail)
      }

    def fromOps[A](operations: Iterable[Language#Operation[A]]): Stream[A] =
      operations.headOption match {
        case None    => empty
        case Some(h) => from(h) ++ fromOps(operations.tail)
      }

    private def readBuffer(buffer: ByteBuffer): Array[Byte] =
      if (buffer.remaining() == 0) {
        buffer.array()
      } else {
        val arr = new Array[Byte](buffer.position())
        buffer.position(0)
        buffer.get(arr, 0, arr.length)
        arr
      }

    private def asyncCompletion(sus: Suspended[Array[Byte]]): CompletionHandler[Integer, ByteBuffer] =
      new CompletionHandler[Integer, ByteBuffer] {
        override def completed(count: Integer, buf: ByteBuffer): Unit =
          if (count == 0) {
            throw new IllegalArgumentException("Invalid asynchronous read completion")
          } else if (count == -1) {
            sus.proceed(new Array[Byte](0))
          } else {
            val result = readBuffer(buf)
            sus.proceed(result)
          }

        override def failed(err: Throwable, buf: ByteBuffer): Unit =
          sus.fail(err)
      }

    def from(channel: AsynchronousByteChannel, chunkSize: Int): Stream[Array[Byte]] =
      from(effect[Array[Byte]] { sus =>
        val buf = ByteBuffer.allocate(chunkSize)
        channel.read(buf, buf, asyncCompletion(sus))
        None
      }).repeat.takeWhileNotZeroLength

    private def longsFrom(i: Long, chunkSize: Int): Stream[Long] = Stream(i) ++ longsFrom(i + chunkSize, chunkSize)

    def fromFile(channel: AsynchronousFileChannel, chunkSize: Int): Stream[Array[Byte]] =
      longsFrom(0, chunkSize)
        .through(index =>
          effect[Array[Byte]] { sus =>
            val buf = ByteBuffer.allocate(chunkSize)
            channel.read(buf, index, buf, asyncCompletion(sus))
            None
          }
        )
        .repeat
        .takeWhileNotZeroLength
  }

  def fromProgram[A](program: Program[A]): Stream[A]
  def fromOp[A](op: Language#Operation[A]): Stream[A]
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
      stream
        .map(ev)
        .takeUntil(_.isEmpty)
        .filter(_.isDefined)
        .map(_.get)

    def takeWhileNotEmpty(implicit ev: A <:< Iterable[_]): Stream[A] =
      stream.takeUntil(ev(_).isEmpty).filter(!ev(_).isEmpty)

    def takeWhileNotZeroLength(implicit ev: A <:< Array[_]): Stream[A] =
      stream.takeUntil(_.length == 0).filter(_.length != 0)

    def effect[B](f: A => Program[_]): Stream[A] = stream.through(a => f(a).map(_ => a))
  }

  implicit def ops[A](stream: Stream[A]): StreamOps[A]

  case class StreamData[A](value: A, next: Stream[A]) {
    override def toString = s"StreamData($value, ${next.getClass.getName})"
  }
  case class ReadStream[A](stream: Stream[A]) extends Op[Option[StreamData[A]]] {
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
