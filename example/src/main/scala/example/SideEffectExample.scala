package example

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

import sanoitus._
import sanoitus.util.LoggingExecutionService

object SideEffectExample {

  def main(args: Array[String]): Unit = {

    val promise = Promise[Execution[_]]()
    promise.future.onComplete(_.foreach { execution =>
      Thread.sleep(1000)
      execution.proceed(8)
    })

    val program =
      for {
        value1 <- unit(5)
        value2 <- effect { execution =>
          promise.success(execution)
          None: Option[Int]
        }
      } yield value1 * value2

    val es = LoggingExecutionService()
    val result = es.execute(program)

    println(result.meta)
    println(result.value)

    es.shutdown()
  }
}
