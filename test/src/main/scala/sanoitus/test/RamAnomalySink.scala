package sanoitus.test

import sanoitus._
import sanoitus.util._

class RamAnomalySink extends AnomalySink {

  trait Error {
    val execution: Execution[_]
    val err: Throwable
  }

  val events = atom(List[Error]())

  override def error[A](_execution: Execution[A], _err: Throwable): Unit =
    cas(events) {
      new Error {
        override val execution = _execution
        override val err = _err
      } :: _
    }

  def clear() = cas(events) { _ =>
    List()
  }
}
