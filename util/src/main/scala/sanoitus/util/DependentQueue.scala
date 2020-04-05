package sanoitus.util

trait DependentQueue[F[_, _], A, B] {
  def enqueue[C](element: F[B, C]): DependentQueue[F, A, C]
  def dequeue: DependentDeque[F, A, B]
}

sealed trait DependentDeque[F[_, _], A, B] {
  def take: Either[DependentDequeElement[F, A, B], F[A, B]]
}

trait DependentDequeElement[F[_, _], A, B] {
  type X0
  val head: F[A, X0]
  val tail: DependentDeque[F, X0, B]
}

case class UnitQueue[F[_, _], A, B](f: F[A, B]) extends DependentQueue[F, A, B] {
  override def enqueue[C](g: F[B, C]): DependentQueue[F, A, C] = LinkedQueue(this, g)
  override def dequeue = UnitDeque(f)
}

case class LinkedQueue[F[_, _], A, B, C](head: DependentQueue[F, A, B], f: F[B, C]) extends DependentQueue[F, A, C] {
  override def enqueue[D](f: F[C, D]): DependentQueue[F, A, D] = LinkedQueue(this, f)
  override def dequeue = DependentQueue.reverse(this)
}

case class UnitDeque[F[_, _], A, B](f: F[A, B]) extends DependentDeque[F, A, B] {
  override def take = Right(f)
}

case class LinkedDeque[F[_, _], A, B, X](f: F[A, X], _tail: DependentDeque[F, X, B]) extends DependentDeque[F, A, B] {
  override def take =
    Left(new DependentDequeElement[F, A, B] {
      override type X0 = X
      override val head = f
      override val tail = _tail
    })
}

object DependentQueue {

  private trait Accumulator[F[_, _], A, B] {
    type X
    val queue: DependentQueue[F, A, X]
    val deque: DependentDeque[F, X, B]
  }

  private object Accumulator {
    def apply[F[_, _], A, B, X0](_queue: DependentQueue[F, A, X0],
                                 _deque: DependentDeque[F, X0, B]): Accumulator[F, A, B] =
      new Accumulator[F, A, B] {
        override type X = X0
        override val queue = _queue
        override val deque = _deque
      }
  }

  @annotation.tailrec
  private def reverse[F[_, _], A, B](acc: Accumulator[F, A, B]): DependentDeque[F, A, B] =
    acc.queue match {
      case UnitQueue(f)         => LinkedDeque(f, acc.deque)
      case LinkedQueue(head, f) => reverse(Accumulator(head, LinkedDeque(f, acc.deque)))
    }

  def reverse[F[_, _], A, B](queue: DependentQueue[F, A, B]): DependentDeque[F, A, B] =
    queue match {
      case UnitQueue(f)               => UnitDeque(f)
      case r: LinkedQueue[F, A, x, B] => reverse(Accumulator(r.head, UnitDeque(r.f)))
    }

  def apply[F[_, _], A, B](a: F[A, B]): DependentQueue[F, A, B] = UnitQueue(a)
}
