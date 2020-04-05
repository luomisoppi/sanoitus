package sanoitus.util

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
