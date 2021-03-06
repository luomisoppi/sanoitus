package example

import java.util.concurrent.atomic.AtomicReference

import sanoitus._

object MyInterpreter extends Interpreter with MyLanguage {

  val store = new AtomicReference(Map[Int, String]())

  override def apply[A](op: Op[A]): Program[A] =
    op match {
      case WriteValue(key, value) => {
        effect[Unit] { _ =>
          store.updateAndGet(_ + ((key, value)))
          Some(())
        }
      }

      case ReadValue(key) => {
        effect[A] { _ => Some(store.get().get(key)) }
      }
    }

  override def close(): Unit = ()
}
