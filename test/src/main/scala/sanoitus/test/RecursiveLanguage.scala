package sanoitus.test

import sanoitus._

trait RecursiveLanguage extends Language { self: Interpreter =>
  sealed trait Op[+A] extends Operation[A]

  case class Sum(acc: Long, value: Int) extends Op[Long]
}
