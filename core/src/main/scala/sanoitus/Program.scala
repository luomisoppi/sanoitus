package sanoitus

private[sanoitus] trait UnitProgram[+A] extends Program[A]

private[sanoitus] case class Return[+A](value: A, map: Boolean = false) extends UnitProgram[A]

private[sanoitus] case class Interpret[A](op: Op[A]) extends UnitProgram[A]

private[sanoitus] case class Directive[+A, B, E[_]](f: E[B] => Option[(E[B], Program[A])]) extends UnitProgram[A]

private[sanoitus] case class Flatmap[A, +B](h: Program[A], f: A => Program[B]) extends Program[B]

trait Program[+A] {
  def flatMap[B](f: A => Program[B]): Program[B] = Flatmap(this, f)
  def map[B](f: A => B): Program[B] = Flatmap(this, (a: A) => Return(f(a), true))
}
