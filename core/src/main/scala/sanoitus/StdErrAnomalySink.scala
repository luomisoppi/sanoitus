package sanoitus

object StdErrAnomalySink extends AnomalySink {
  def error[A](exec: Execution[A], err: Throwable): Unit = {
    System.err.println(exec)
    err.printStackTrace()
  }
}
