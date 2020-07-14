package example

import sanoitus._
import sanoitus.parallel.core.ParallelInterpreter._

object ParallelExample2 {

  def main(args: Array[String]): Unit = {

    val program = for {
      res <- resource("resource") { value => println(s"Closing $value") }
      _ <- Fork(unit(()), res)
      _ <- effect[Any] { _ => None }
    } yield ()

    val es = BasicExecutionService()

    // will print "Closing resource", even if the "main" program never ends
    es.execute(program)
  }
}
