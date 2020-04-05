package example

import sanoitus._

trait MyLanguage extends Language { self: Interpreter =>
  case class WriteValue(key: Int, value: String) extends Operation[Unit]
  case class ReadValue(key: Int) extends Operation[Option[String]]
}
