package sanoitus

trait AnomalySink {
  def error[A](exec: Execution[A], err: Throwable): Unit
}
