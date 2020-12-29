package example

import sanoitus._

trait MyLanguage extends Language { self: Interpreter =>
  sealed trait Op[+A] extends Operation[A]

  case class WriteValue(key: Int, value: String) extends Op[Unit]
  case class ReadValue(key: Int) extends Op[Option[String]]
}
