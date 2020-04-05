package sanoitus.test
import sanoitus._

object TestLanguageInterpreter extends TestLanguage with Interpreter {

  override def close() = ()

  override def apply[A](op: Operation[A]): Program[A] =
    op match {
      case ReturnValue(value) => unit(value)
      case FailWith(err)      => throw err
    }
}
