package example

import sanoitus._
import sanoitus.parallel.core.ParallelInterpreter._

object ParallelExample1 {

  def main(args: Array[String]): Unit = {
    val readThreadName = effect[String] { _ => Some(Thread.currentThread.getName()) }

    val program = for {
      reader1 <- Fork(readThreadName)
      reader2 <- Fork(readThreadName)
      main <- readThreadName
      name1 <- Await(reader1)
      name2 <- Await(reader2)
    } yield (main, name1, name2)

    val es = BasicExecutionService()

    // will print something like "(pool-1-thread-1,pool-1-thread-2,pool-1-thread-3)"
    println(es.executeUnsafe(program))

    es.shutdown()
  }
}
