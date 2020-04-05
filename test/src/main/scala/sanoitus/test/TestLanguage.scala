package sanoitus.test

import sanoitus._

trait TestLanguage extends Language { self: Interpreter =>
  case class ReturnValue[A](a: A) extends Operation[A]
  case class FailWith(err: Throwable) extends Operation[Unit]
}
