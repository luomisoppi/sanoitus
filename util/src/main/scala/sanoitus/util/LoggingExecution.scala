package sanoitus.util

import sanoitus._

case class LoggingExecution[A](
  val program: Program[A],
  override val callback: ExecutionResult[A, Log] => Unit,
  override val resources: Set[Resource[_]],
  override val es: LoggingExecutionService,
  entries: List[LogEntry] = List(),
  start: Long = System.nanoTime(),
  depth: Int = 0
) extends Execution[A] {

  override type Meta = Log
  override type Self = LoggingExecution[A]
  override val meta = Log(entries)

  override def mapResources(f: Set[Resource[_]] => Set[Resource[_]]) =
    LoggingExecution(program, callback, f(resources), es, entries, start, depth)

  override def continueWith(value: Either[Throwable, Any]): Unit = es.continue(this, value)

  def setProgram(prog: Program[A]) =
    LoggingExecution(prog, callback, resources, es, entries, start, depth)

  def addEntry(entry: LogEntry) =
    LoggingExecution(program, callback, resources, es, entry :: entries, start, depth)

  def addOp(op: Op[_]) = addEntry(CallEntry(op, Thread.currentThread().getName, System.nanoTime() - start, depth))

  def addReturn(value: Any) =
    if (value.isInstanceOf[Array[_]]) {
      addEntry(
        ReturnEntry(
          "<array of length " + value.asInstanceOf[Array[_]].length + ">",
          Thread.currentThread().getName,
          System.nanoTime() - start,
          depth
        )
      )
    } else {
      addEntry(ReturnEntry(value, Thread.currentThread().getName, System.nanoTime() - start, depth))
    }

  def addEffect() = addEntry(EffectEntry(Thread.currentThread().getName, System.nanoTime() - start, depth))

  def addMapResources(pre: Set[Resource[_]], post: Set[Resource[_]]) =
    addEntry(MapResourcesEntry(pre, post, Thread.currentThread().getName, System.nanoTime() - start, depth))

  def addMap(value: A) =
    LoggingExecution(
      program,
      callback,
      resources,
      es,
      entries.head.addMap(value) :: entries.tail,
      start,
      depth
    )

  def decDepth =
    LoggingExecution(program, callback, resources, es, entries, start, depth - 1)
  def incDepth =
    LoggingExecution(program, callback, resources, es, entries, start, depth + 1)

  override def toString = meta.toString
}
