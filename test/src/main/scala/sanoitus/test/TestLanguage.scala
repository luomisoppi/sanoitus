package sanoitus.test

import sanoitus._

trait TestLanguage extends Language { self: Interpreter =>
  sealed trait Op[+A] extends Operation[A]

  case class ReturnValue[A](a: A) extends Op[A]
  case class FailWith(err: Throwable) extends Op[Unit]
}
