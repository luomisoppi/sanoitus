package sanoitus

trait Interpreter extends Language {
  def apply[A](op: Operation[A]): Program[A]
  def close(): Unit
}
