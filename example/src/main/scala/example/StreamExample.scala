package example

import sanoitus._
import sanoitus.stream.core.StreamInterpreter._

object StreamExample {

  def integersFrom(i: Int): Stream[Int] = Stream(i) ++ integersFrom(i + 1)

  def main(args: Array[String]): Unit = {

    val es = BasicExecutionService()

    val stream = integersFrom(1).take(5)

    // will print "List(1, 2, 3, 4, 5)"
    println(es.executeUnsafe(ReadAll(stream)))

    val streamWithSideEffects =
      stream.through(value => effect { _ => Some(println(s"value from stream: $value")) })
    // will print:
    // value from stream: 1
    // value from stream: 2
    // value from stream: 3
    // value from stream: 4
    // value from stream: 5
    es.executeUnsafe(Process(streamWithSideEffects))

    es.shutdown()
  }
}
