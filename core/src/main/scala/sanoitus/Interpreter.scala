package sanoitus

trait Interpreter extends Language {
  def apply[A](op: Op[A]): Program[A]
  def close(): Unit
}
