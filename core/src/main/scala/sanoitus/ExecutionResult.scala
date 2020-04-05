package sanoitus

trait ExecutionResult[+A, +Meta] {
  val value: Either[Throwable, A]
  val meta: Meta

  override def toString = s"ExecutionResult($value)"
}
