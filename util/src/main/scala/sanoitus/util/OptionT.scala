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

object OptionT extends MonadTransformer {

  implicit def liftOpToOptionT[A](op: Language#Operation[Option[A]]): OptionT[Program, A] = new OptionT(op)

  implicit def liftProgramToOptionT[A](prog: Program[Option[A]]): OptionT[Program, A] = new OptionT(prog)

  implicit def liftNonOptionProgramToOptionT[A: |¬|[Option[_]]#λ, L](value: Program[A]): OptionT[Program, A] =
    new OptionT(value.map(Some(_): Option[A]))

  implicit class OpOptionT[A](value: Language#Operation[Option[A]]) {
    def optT: OptionT[Program, A] = new OptionT(value)
  }

  implicit class ProgramOptionT[A](value: Program[Option[A]]) {
    def optT: OptionT[Program, A] = new OptionT(value)
  }

  implicit class OpNotOptionT[A: |¬|[Option[_]]#λ](value: Language#Operation[A]) {
    def optT: OptionT[Program, A] = new OptionT(value.map(Some(_): Option[A]))
  }

  implicit class ProgramNotOptionT[A: |¬|[Option[_]]#λ](value: Program[A]) {
    def optT: OptionT[Program, A] = new OptionT(value.map(Some(_): Option[A]))
  }

  implicit def toProgram[A](optionT: OptionT[Program, A]): Program[Option[A]] =
    optionT.value
}
