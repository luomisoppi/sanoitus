package sanoitus.test.parallel

import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit

import org.scalatest.funsuite.AnyFunSuite

import sanoitus._
import sanoitus.parallel._
import sanoitus.test._
import sanoitus.util._

trait ParallelLanguageTest extends AnyFunSuite {

  val es: ExecutionService

  val sink = new RamAnomalySink()

  implicit val language: ParallelLanguage
  import language._

  implicit val testLanguage: TestLanguage = TestLanguageInterpreter
  import testLanguage._

  test("Exceeding threadpool queue size does not block processing") {

    def program(value: Int): Program[Int] =
      if (value == 0) {
        unit(1)
      } else {
        for {
          futX <- Fork(program(value - 1))
          futY <- Fork(program(value - 1))
          x <- Await(futX)
          y <- Await(futY)
        } yield x + y
      }

    val result = es.executeUnsafe(program(20))
    assert(result == scala.math.pow(2, 20))
  }

  test("Successful forked execution behaves as expected") {
    def program(value: Int) =
      for {
        a <- ReturnValue(value)
        b <- unit(value)
        c <- effect[Int] { _ => Some(value) }
      } yield a + b + c

    val forkingProgram =
      for {
        futA <- Fork(program(1))
        futB <- Fork(program(2))
        a <- Await(futA)
        b <- Await(futB)
      } yield ((a, b))

    val result = es.execute(forkingProgram)
    result.value match {
      case Left(err) => err.printStackTrace()
      case _         => ()
    }
    assert(result.value == Right((3, 6)))
  }

  test("Exceptions in interpreters of forked programs returned properly") {
    val err = new IllegalStateException("failure")
    val failingProgram =
      for {
        _ <- FailWith(err)
      } yield "never returned"

    val main =
      for {
        futX <- Fork(failingProgram)
        x <- Await(futX)
      } yield x

    val result = es.execute(main)
    assert(result.value == Left(err))
  }

  test("Exceptions in effects of forked programs returned properly") {
    val err = new IllegalStateException("failure")
    val failingProgram =
      for {
        _ <- effect[String] { _ => throw err }
      } yield "never returned"

    val main =
      for {
        futX <- Fork(failingProgram)
        x <- Await(futX)
      } yield x

    val result = es.execute(main)
    assert(result.value == Left(err))
  }

  test("Exceptions in logic of forked programs returned properly") {
    val err = new IllegalStateException("failure")
    val failingProgram =
      for {
        _ <- ReturnValue(1)
        _ = throw err
      } yield "never returned"

    val main =
      for {
        futX <- Fork(failingProgram)
        x <- Await(futX)
      } yield x

    val result = es.execute(main)
    assert(result.value == Left(err))
  }

  test("Big promise flatmap works (tail heavy)") {

    val CountTo: Long = 100000

    def promise(count: Long): Program[Promise[Long]] =
      if (count == 0) {
        Fork(unit(0L))
      } else {
        for {
          value <- Fork(unit(count))
          value2 <- promise(count - 1)
        } yield for {
          a <- value
          b <- value2
        } yield a + b
      }

    val main: Program[Long] =
      for {
        prom <- promise(CountTo)
        res <- Await(prom)
      } yield res

    val result = es.executeUnsafe(main)
    assert(result == CountTo * (CountTo + 1) / 2)
  }

  test("Big promise flatmap works (front heavy)") {

    val CountTo: Long = 100000

    def promise(count: Long): Program[Promise[Long]] =
      if (count == 0) {
        Fork(unit(0L))
      } else {
        for {
          value <- Fork(unit(count))
          value2 <- promise(count - 1)
        } yield for {
          b <- value2
          a <- value
        } yield a + b
      }

    val main =
      for {
        prom <- promise(CountTo)
        res <- Await(prom)
      } yield res

    val result = es.executeUnsafe(main)
    assert(result == CountTo * (CountTo + 1) / 2)
  }

  test("Resource transfer behaves as expected") {
    val resource = new Semaphore(0)
    def closer(s: Semaphore) = s.release(2)

    def program(value: Int) =
      for {
        a <- ReturnValue(value)
      } yield a

    val main =
      for {
        ab <- resources(resource)(closer)
        _ <- Fork(program(1), ab(0))
        _ <- effect[Unit] { _ => None }
      } yield ()

    es.executeAsync(main)(_ => ())
    resource.tryAcquire(1, TimeUnit.SECONDS)
    assert(resource.availablePermits() == 1)
  }

  test("Illegal fork causes program to be terminated and anomaly sink to be notified") {

    def illegal(res: Resource[Unit]): Program[Unit] =
      for {
        _ <- Fork(unit(()), res)
      } yield ()

    val program =
      for {
        res <- resource(())(_ => ())
        promise <- Fork(illegal(res))
        _ <- Await(promise)
      } yield "ok"

    val result = es.execute(program)
    assert(result.value == Left(IllegalFork) && sink.events.get.head.err == IllegalFork)
  }

  test("Creating and fulfilling promises") {

    def fulfiller(promise: WritablePromise[Int]): Program[Unit] =
      for {
        _ <- effect[Unit] { _ => Some(Thread.sleep(1000)) }
        _ <- FulfillPromise(promise, Right(15))
      } yield ()

    val program =
      for {
        promise <- CreatePromise[Int]()
        _ <- Fork(fulfiller(promise))
        result <- Await(promise)
      } yield result

    val result = es.execute(program)
    assert(result.value == Right(15))
  }
}
