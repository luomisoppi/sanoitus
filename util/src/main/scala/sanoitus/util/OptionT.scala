package sanoitus
package util

class OptionT[M[_], A](val value: M[Option[A]])(implicit val mon: Monad[M]) {

  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
    new OptionT(mon.flatMap(value) {
      case Some(a) => f(a).value
      case None    => mon.unit(None)
    })

  def map[B](f: A => B): OptionT[M, B] =
    new OptionT(mon.map(value) {
      case Some(a) => Some(f(a))
      case None    => None
    })
}

object OptionT {

  // Thanks, Miles! (https://gist.github.com/milessabin/c9f8befa932d98dcc7a4)
  trait <:!<[A, B]

  implicit def nsub[A, B]: A <:!< B = null
  implicit def nsubAmbig1[A, B >: A]: A <:!< B = null
  implicit def nsubAmbig2[A, B >: A]: A <:!< B = null

  type |¬|[T] = {
    type λ[U] = U <:!< T
  }

  implicit def liftOpToOptionT[A](op: Language#Operation[Option[A]]): OptionT[Program, A] = new OptionT(op)

  implicit class ProgramNotOptionT[A: |¬|[Option[_]]#λ](value: Program[A]) {
    def optT: OptionT[Program, A] = new OptionT(value.map(Some(_): Option[A]))
  }

  implicit class ProgramOptionT[A](value: Program[Option[A]]) {
    def optT: OptionT[Program, A] = new OptionT(value)
  }

  implicit class OpNotOptionT[A: |¬|[Option[_]]#λ](value: Language#Operation[A]) {
    def optT: OptionT[Program, A] = new OptionT(value.map(Some(_): Option[A]))
  }

  implicit class OpOptionT[A](value: Language#Operation[Option[A]]) {
    def optT: OptionT[Program, A] = new OptionT(value)
  }

  implicit def toProgram[A](optionT: OptionT[Program, A]): Program[Option[A]] =
    optionT.value
}
