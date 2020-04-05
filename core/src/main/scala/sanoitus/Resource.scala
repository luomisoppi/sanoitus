package sanoitus

trait Resource[A] {

  val value: A

  val index: Option[Int]

  private[sanoitus] def close(): Program[Unit]

  override def toString = index match {
    case None    => s"Resource($value)"
    case Some(i) => s"Resource(value=$value, index=$i)"
  }
}
