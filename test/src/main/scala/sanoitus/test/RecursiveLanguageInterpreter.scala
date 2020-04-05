package sanoitus.test
import sanoitus._

object RecursiveLanguageInterpreter extends RecursiveLanguage with Interpreter {
  override def close() = ()

  override def apply[A](op: Operation[A]): Program[A] =
    op match {
      case Sum(acc, value) => if (value == 0) unit(acc) else Sum(acc + value, value - 1)
    }
}
