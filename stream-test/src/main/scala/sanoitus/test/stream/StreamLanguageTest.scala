package sanoitus.test.stream

import org.scalatest.funsuite.AnyFunSuite

import sanoitus._
import sanoitus.stream._

trait StreamLanguageTest extends AnyFunSuite {

  val es: ExecutionService

  implicit val language: StreamLanguage
  import language._

  def integersFrom(i: Int): Stream[Int] = Stream(i) ++ integersFrom(i + 1)

  test("Concatenation works") {
    val program = ReadAll(Stream(1) ++ Stream(2) ++ Stream(3))

    val result = es.executeUnsafe(program)

    assert(result == Seq(1, 2, 3))
  }

  test("Take works") {
    val program = ReadAll(integersFrom(1).take(10))

    val result = es.executeUnsafe(program)

    assert(result == Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  test("TakeWhileDefined works") {
    val stream: Stream[Option[Int]] = integersFrom(1).take(10).map(Some(_)) ++ Stream(None)
    val program = ReadAll(stream.takeWhileDefined)

    val result = es.executeUnsafe(program)

    assert(result == Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  test("Drop works") {
    val program = ReadAll(integersFrom(1).drop(10).take(5))

    val result = es.executeUnsafe(program)

    assert(result == Seq(11, 12, 13, 14, 15))
  }

  test("DropWhile works") {
    val program = ReadAll(integersFrom(1).dropWhile(_ <= 10).take(5))

    val result = es.executeUnsafe(program)

    assert(result == Seq(11, 12, 13, 14, 15))
  }

  test("Flatmap works") {
    val stream =
      for {
        x <- integersFrom(1).take(5)
        y <- Stream(x * 2 - 1) ++ Stream(x * 2)
      } yield y

    val result = es.executeUnsafe(ReadAll(stream))

    assert(result == Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  test("Filter works") {
    val stream = integersFrom(1).take(10).filter(_ % 2 == 0)

    val result = es.executeUnsafe(ReadAll(stream))

    assert(result == Seq(2, 4, 6, 8, 10))
  }

  test("Fold works") {
    val stream = integersFrom(1).take(10).foldLeft(0)(_ + _)

    val result = es.executeUnsafe(ReadAll(stream))

    assert(result == Seq(55))
  }

  test("Zip works") {
    val stream = integersFrom(1)
      .zip(integersFrom(6))
      .map(x => x._1 + x._2)
      .take(5)
      .foldLeft(0)(_ + _)

    val result = es.executeUnsafe(ReadAll(stream))

    assert(result == Seq(55))
  }

  test("TakeUntil works") {
    val stream = integersFrom(1)
      .takeUntil(_ == 5)

    val result = es.executeUnsafe(ReadAll(stream))

    assert(result == Seq(1, 2, 3, 4, 5))
  }

  test("Massive fold works") {
    val stream = integersFrom(1).take(1000000).foldLeft(0L)(_ + _)

    val result = es.executeUnsafe(ReadAll(stream))

    assert(result == Seq(500000500000L))
  }

  test("Effect works") {
    var x = 0
    val stream = integersFrom(1)
      .take(5)
      .effect { a =>
        x += a
        a * 2
      }
      .take(3)

    val result = es.executeUnsafe(ReadAll(stream))

    assert(x == 6)
    assert(result == Seq(2, 4, 6))
  }

}
