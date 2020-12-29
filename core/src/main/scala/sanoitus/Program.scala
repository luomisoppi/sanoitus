package sanoitus

private[sanoitus] trait UnitProgram[+A] extends Program[A]

private[sanoitus] case class Return[+A](value: A, map: Boolean = false) extends UnitProgram[A]

private[sanoitus] case class Interpret[+A](op: Op[A]) extends UnitProgram[A]

private[sanoitus] case class Effect[+A, S <: A](s: Suspended[S] => Option[A]) extends UnitProgram[A]

private[sanoitus] case class MapResources(f: Set[Resource[_]] => Set[Resource[_]]) extends UnitProgram[Unit]

private[sanoitus] case class PeekResources() extends UnitProgram[Set[Resource[_]]]

private[sanoitus] case class Flatmap[A, +B](h: Program[A], f: A => Program[B]) extends Program[B]

private[sanoitus] case class RightBoundFlatmap[A, +B](h: UnitProgram[A], f: A => Program[B]) extends Program[B]

sealed trait Program[+A] {
  def flatMap[B](f: A => Program[B]): Program[B] = Flatmap(this, f)
  def map[B](f: A => B): Program[B] = Flatmap(this, (a: A) => Return(f(a), true))
}
