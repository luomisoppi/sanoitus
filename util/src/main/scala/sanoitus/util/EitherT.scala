package sanoitus
package util

class EitherT[M[_], L, R](val value: M[Either[L, R]])(implicit val mon: Monad[M]) {

  def flatMap[R2](f: R => EitherT[M, L, R2]): EitherT[M, L, R2] =
    new EitherT(mon.flatMap(value) {
      case Right(r) => f(r).value
      case Left(l)  => mon.unit(Left(l))
    })

  def leftMap[L2](f: L => L2): EitherT[M, L2, R] =
    new EitherT(mon.map(value) {
      case Right(r) => Right(r)
      case Left(l)  => Left(f(l))
    })

  def map[R2](f: R => R2): EitherT[M, L, R2] =
    new EitherT(mon.map(value) {
      case Right(r) => Right(f(r))
      case Left(l)  => Left(l)
    })

  def merge[A](implicit evl: L <:< A, evr: R <:< A): M[A] = mon.map(value)(_.map(evr(_)).left.map(evl(_)).merge)
}

object EitherT extends MonadTransformer {

  implicit def liftOpToEitherT[L, R](op: Language#Operation[Either[L, R]]): EitherT[Program, L, R] = new EitherT(op)

  implicit def liftProgramToEitherT[L, R](prog: Program[Either[L, R]]): EitherT[Program, L, R] = new EitherT(prog)

  implicit def liftNonEitherProgramToEitherT[A: |¬|[Either[_, _]]#λ, L](value: Program[A]): EitherT[Program, L, A] =
    new EitherT(value.map(Right(_): Either[L, A]))

  implicit class ProgramEitherT[L, R](value: Program[Either[L, R]]) {
    def eitherT: EitherT[Program, L, R] = new EitherT(value)
  }

  implicit class OpEitherT[L, R](value: Language#Operation[Either[L, R]]) {
    def eitherT: EitherT[Program, L, R] = new EitherT(value)
  }

  implicit def toProgram[L, R](eitherT: EitherT[Program, L, R]): Program[Either[L, R]] =
    eitherT.value
}
