package sanoitus.test

import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.funsuite.AnyFunSuite

import sanoitus._

trait ExecutionServiceTest extends AnyFunSuite {

  val es: ExecutionService

  val sink = new RamAnomalySink()

  val test: TestLanguage = TestLanguageInterpreter
  import test._

  val recursive: RecursiveLanguage = RecursiveLanguageInterpreter
  import recursive._

  test("Successful unit program behaves as expected") {
    val program = unit("a")
    val result = es.executeUnsafe(program)
    assert(result == "a")
  }

  test("Successful interpreter program behaves as expected") {
    val program = ReturnValue("a")
    val result = es.executeUnsafe(program)
    assert(result == "a")
  }

  test("Successful effect program behaves as expected") {
    val program = effect[String] { _ => Some("a") }
    val result = es.executeUnsafe(program)
    assert(result == "a")
  }

  test("Successful mapResources program behaves as expected") {
    val program = mapResources(identity)
    val result = es.execute(program)
    assert(result.value.isRight == true)
  }

  test("Successful peekResources program behaves as expected") {
    val program = peekResources
    val result = es.executeUnsafe(program)
    assert(result == Set())
  }

  test("Successful composed program behaves as expected") {
    val program =
      for {
        a <- ReturnValue(1)
        b <- unit("a")
        c <- effect[Option[String]] { _ => Some(Some("value")) }
      } yield (a, b, c)

    val result = es.executeUnsafe(program)
    assert(result == ((1, "a", Some("value"))))
  }

  test("Effect returning None leaves execution unfinished") {
    val sem = new Semaphore(0)
    val program =
      for {
        a <- effect[String] { _ =>
          sem.release()
          None
        }
      } yield a

    var callbackExecuted = false
    es.executeAsync(program)(_ => callbackExecuted = true)
    sem.tryAcquire(1, TimeUnit.SECONDS)
    assert(callbackExecuted == false)
  }

  test("Exceptions in interpreters handled okay") {
    val err = new IllegalStateException("failure")
    val program =
      for {
        _ <- FailWith(err)
      } yield ()

    val result = es.execute(program)
    assert(result.value == Left(err))
  }

  test("Exceptions in effects handled okay") {
    val err = new IllegalStateException("failure")
    val program =
      for {
        _ <- effect[Unit] { _ => throw err }
      } yield ()

    val result = es.execute(program)
    assert(result.value == Left(err))
  }

  test("Exceptions in program logic handled okay") {
    val err = new IllegalStateException("failure")
    val program =
      for {
        x <- ReturnValue(0)
        _ = throw err
      } yield x

    val result = es.execute(program)
    assert(result.value == Left(err))
  }

  test("Stack okay with big front heavy program (operations)") {
    val program = (1 to 1000000).foldLeft(unit(List[Int]())) { (acc, a) =>
      for {
        list <- acc
        x <- ReturnValue(a)
      } yield x :: list
    }

    val result = es.executeUnsafe(program)
    assert(result.size == 1000000)
  }

  test("Stack okay with big tail heavy program (operations)") {
    val program = (1 to 1000000).foldLeft(unit(List[Int]())) { (acc, a) =>
      for {
        x <- ReturnValue(a)
        list <- acc
      } yield x :: list
    }

    val result = es.executeUnsafe(program)
    assert(result.size == 1000000)
  }

  test("Stack okay with big front heavy program (effects)") {
    val program = (1 to 1000000).foldLeft(unit(List[Int]())) { (acc, a) =>
      for {
        list <- acc
        x <- effect[Int](_ => Some(a))
      } yield x :: list
    }

    val result = es.executeUnsafe(program)
    assert(result.size == 1000000)
  }

  test("Stack okay with big tail heavy program (effects)") {
    val program = (1 to 1000000).foldLeft(unit(List[Int]())) { (acc, a) =>
      for {
        x <- effect[Int](_ => Some(a))
        list <- acc
      } yield x :: list
    }

    val result = es.executeUnsafe(program)
    assert(result.size == 1000000)
  }

  test("Stack okay with big recursive interpretation") {
    val program = for {
      sum <- Sum(0, 1000000)
    } yield sum

    val result = es.executeUnsafe(program)
    assert(result == 500000500000L)
  }

  test("Closing of resources works") {
    val createResource = new AtomicInteger()
    def closer1(a: AtomicInteger): Unit = a.addAndGet(1)
    def closer2(a: AtomicInteger): Unit = a.addAndGet(2)

    val program =
      for {
        ab <- resources(createResource)(closer1, closer2)
        _ <- close(ab(0))
        _ <- close(ab(1))
      } yield ()

    es.executeUnsafe(program)
    assert(createResource.get == 3)
  }

  test("Automatic closing of resources works") {
    val createResource = new AtomicInteger()
    def closer1(a: AtomicInteger) = a.addAndGet(1)
    def closer2(a: AtomicInteger) = a.addAndGet(2)

    val program =
      for {
        ab <- resources(createResource)(closer1, closer2)
        _ <- close(ab(0))
      } yield ()

    es.execute(program)
    assert(createResource.get == 3)
  }

  test("Resource close failure does not affect automatic closing of other resources") {
    val resource = new AtomicInteger(0)
    val closer1 = (_: AtomicInteger) => throw new IllegalStateException()
    def closer2(a: AtomicInteger) = a.incrementAndGet()
    val closer3 = (_: AtomicInteger) => throw new NullPointerException()

    val main =
      for {
        _ <- resources(resource)(closer1, closer2, closer3)
      } yield ()

    es.execute(main)
    assert(resource.get == 1)
  }

  test("Failing execution sent to anomaly sink") {
    val failure = new NullPointerException()

    val program =
      for {
        _ <- FailWith(failure)
      } yield ()

    es.execute(program)

    assert(sink.events.get.head.err == failure)
  }

  test("Failing autoclose sent to anomaly sink, but program still returns a value") {
    val failure = new NullPointerException()
    val closer: Unit => Unit = _ => throw failure

    val program =
      for {
        _ <- resource(())(closer)
      } yield "ok"

    val result = es.executeUnsafe(program)
    assert(result == "ok" && sink.events.get.head.err == new AutoCloseFailed(failure))
  }

  test("Closing a resource that does not belong to the execution does nothing") {
    val closeCount = new AtomicInteger(0)
    val program =
      for {
        res <- resource(closeCount)(_.incrementAndGet())
        _ <- close(res)
        _ <- close(res)
      } yield "ok"

    val result = es.executeUnsafe(program)
    assert(result == "ok" && closeCount.intValue() == 1)
  }

  test("Continuing suspended execution") {
    val program =
      for {
        res <- effect[Int] { e =>
          e.continueWith(Right(15))
          None
        }
      } yield res

    val res = es.executeUnsafe(program)
    assert(res == 15)
  }
}
