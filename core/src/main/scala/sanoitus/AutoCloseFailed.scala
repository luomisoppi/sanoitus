package sanoitus

class AutoCloseFailed(val reason: Throwable) extends Throwable {
  override def equals(that: Any): Boolean =
    that match {
      case that: AutoCloseFailed => that.reason == reason
      case _                     => false
    }
}
