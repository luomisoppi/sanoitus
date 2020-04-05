package sanoitus.util

import sanoitus._

sealed trait LogEntry {
  val thread: String
  val time: Long
  val depth: Int
  def addMap(mapped: Any): LogEntry
}

case class CallEntry(op: Op[_], thread: String, time: Long, depth: Int) extends LogEntry {
  def addMap(mapped: Any) = ???
}

case class ReturnEntry(value: Any, thread: String, time: Long, depth: Int, maps: List[Any] = List()) extends LogEntry {
  def addMap(mapped: Any) = ReturnEntry(value, thread, time, depth, mapped :: maps)
}

case class DirectiveEntry(thread: String, time: Long, depth: Int) extends LogEntry {
  def addMap(mapped: Any) = ???
}

case class Log(entries: List[LogEntry]) {

  private def prefixOf(entry: LogEntry): String =
    s"${((entry.time / 1000000).toString + "ms").padTo(15, " ").mkString}${entry.thread.padTo(30, " ").mkString}" + ("  " * entry.depth)

  override def toString =
    entries.reverse
      .foldLeft(("", false)) { (acc, entry) =>

        if (acc._2) {
          (acc._1, false)
        } else {
          val prefix = prefixOf(entry)
          val (content, wasEffect) = entry match {
            case entry: CallEntry =>
              (entry.op.toString, false)
            case _: DirectiveEntry => ("[effect]", true)
            case entry: ReturnEntry =>
              ("=> " + entry.maps.reverse
                 .foldLeft(List[Any](entry.value))((acc, a) =>
                   if (a.toString.equals(acc.last.toString)) acc
                   else a :: acc
                 )
                 .reverse
                 .mkString(" => "),
               false)
          }
          (s"${acc._1}\n$prefix $content", wasEffect)
        }
      }
      ._1
}
