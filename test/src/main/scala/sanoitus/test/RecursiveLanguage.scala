package sanoitus.test

import sanoitus._

trait RecursiveLanguage extends Language { self: Interpreter =>
  case class Sum(acc: Long, value: Int) extends Operation[Long]
}
