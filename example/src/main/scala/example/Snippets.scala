package example

import sanoitus._

object Snippet1 {
  def main(args: Array[String]): Unit = {
    val program = unit("Hello world!")
    val es = BasicExecutionService()
    val result = es.executeUnsafe(program)

    // will print "Hello world!"
    println(result)

    es.shutdown()
  }
}

object Snippet2 {
  def main(args: Array[String]): Unit = {
    val es = BasicExecutionService()

    val program =
      for {
        hello <- unit("Hello")
        world <- unit("world")
      } yield (hello, world, "!")

    println(es.executeUnsafe(program))

    es.shutdown()
  }
}

object Snippet3 {
  def main(args: Array[String]): Unit = {
    val program = effect[Unit] { _ => Some(println("Hello world!")) }
    val es = BasicExecutionService()

    // will print "Hello world!"
    es.executeUnsafe(program)

    es.shutdown()
  }
}

object Snippet4 {
  import MyInterpreter._

  def main(args: Array[String]): Unit = {
    val es = BasicExecutionService()

    val program = for {
      _ <- WriteValue(1, "one")
      one <- ReadValue(1)
      two <- ReadValue(2)
    } yield (one, two)

    println(es.executeUnsafe(program))

    es.shutdown()
  }
}

object Snippet5 {
  def main(args: Array[String]): Unit = {
    val es = BasicExecutionService()
    val program = unit("Hello world!")

    val result1: String = es.executeUnsafe(program)
    // will print "Hello World!"
    println(result1)

    val result2: ExecutionResult[String, Unit] = es.execute(program)
    // will print "Right(Hello world!)"
    println(result2.value)

    // will print "ExecutionResult(Right(Hello world!))"
    es.executeAsync(program, println)

    es.shutdown()
  }
}

object Snippet6 {
  def main(args: Array[String]): Unit = {
    val es = BasicExecutionService()

    val program = for {
      res <- resource("resource1") { res => println(s"closing resource $res") }
      _ <- resource("resource2") { res => println(s"closing resource $res") }
      _ <- close(res)
    } yield ()

    // will print "
    //  closing resource resource1
    //  closing resource resource2
    // "
    es.executeUnsafe(program)

    es.shutdown()
  }
}
