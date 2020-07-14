package example

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

import sanoitus._
import sanoitus.util.LoggingExecutionService

object SideEffectExample {

  def main(args: Array[String]): Unit = {

    val promise = Promise[Suspended[Int]]()
    promise.future.onComplete(_.foreach { suspended =>
      Thread.sleep(1000)
      suspended.proceed(8)
    })

    val program =
      for {
        value1 <- unit(5)
        value2 <- effect[Int] { suspended =>
          promise.success(suspended)
          None
        }
      } yield value1 * value2

    val es = LoggingExecutionService()
    val result = es.execute(program)

    println(result.meta)
    println(result.value)

    es.shutdown()
  }
}
