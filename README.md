# Sanoitus

## Overview

Sanoitus is a library for defining and executing programs expressed as Scala values.

A minimal program and its execution:

```scala
import sanoitus._

val program = unit("Hello world!")
val es = BasicExecutionService()
val result = es.executeUnsafe(program)

// will print "Hello world!"
println(result)
```

Programs can be defined using for comprehension:

```scala
val program =
  for {
    hello <- unit("Hello")
    world <- unit("world")
  } yield (hello, world, "!")
```

Programs can have side effects:

```scala
val program = effect[Unit] { _ => Some(println("Hello world!")) }
val es = BasicExecutionService()

// will print "Hello world!"
es.executeUnsafe(program)
```

And most importantly, programs can contain operations from externally defined languages:

```scala
val myLanguage: MyLanguage
import myLanguage._

val program =
  for {
    _ <- WriteValue(1, "one")
    one <- ReadValue(1)
    two <- ReadValue(2)
  } yield (one, two)
```

A language is just a set of operations:

```scala
import sanoitus._

trait MyLanguage extends Language { self: Interpreter =>
  sealed trait Op[+A] extends Operation[A]

  case class WriteValue(key: Int, value: String) extends Op[Unit]
  case class ReadValue(key: Int) extends Op[Option[String]]
}
```

A language can be instantiated as an interpreter. An interpreter translates operations of the language to programs. It does so by using other languages (or maybe even its own language).

In this simplistic example, the only language used in the interpreter implementation is the Sanoitus root language (which has the operations 'unit', 'effect' and a few others):

```scala
import sanoitus._
import java.util.concurrent.atomic.AtomicReference

object MyInterpreter extends Interpreter with MyLanguage {

  val store = new AtomicReference(Map[Int, String]())

  override def apply[A](op: Op[A]): Program[A] =
    op match {
      case WriteValue(key, value) => {
        effect[Unit] { _ =>
          store.updateAndGet(_ + ((key, value)))
          Some(())
        }
      }

      case ReadValue(key) => {
        effect[A] { _ => Some(store.get().get(key)) }
      }
    }

  override def close(): Unit = ()
}
```

Note that a language can have any number of interpreters. In context of this example, there could be another interpreter reading/writing the values from/to files instead of memory. Exactly the same program can cause very different side effects by changing the interpreter(s).

## Execution

Programs are executed by an ExecutionService. This repository has 2 ExecutionService implementations:

* BasicExecutionService - a minimum implementation.
* LoggingExecutionService - stores every interpretation step and their results with the execution for later inspection

There are multiple ways to execute a program:

```scala
val es = BasicExecutionService()
val program = unit("Hello world!")

val result1: String = es.executeUnsafe(program)
// will print "Hello World!"
println(result1)

val result2: ExecutionResult[String, Unit] = es.execute(program)
// will print "Right(Hello world!)"
println(result2.value)

// will print "ExecutionResult(Right(Hello world!))"
es.executeAsync(program)(println)
```

"executeUnsafe" returns the raw result and throws any exception that the execution causes.

"execute" will return an ExecutionResult that consists of:

* value - The result of the execution: either a successfully computed outcome or an exception that broke the execution.
* meta - an accumulated value which the ExecutionService collected. For BasicExecutionService there is none (Unit), for LoggingExecutionService this is a representation of the execution phases.

"executeAsync" will return immediately and, after the execution finishes, call the function given as the second argument with the result of the execution.

## Side effects

Like programs, also their executions are expressed as Scala values. While contents of the execution values are relevant to the ExecutionService implementations only, the values themselves can be accessed when defining side effects in the programs:

```scala
val program = effect[Unit] { suspended => Some(println("Hello world!")) }
```

The value "suspended" above represents the state of the execution at the moment it reaches the 'effect' operation. The return value defines whether the execution should continue. By returning Some("x"), the execution will continue assuming the effect returned value "x". By returning None, the execution will not continue; the ExecutionService will simply forget about it.

But as the execution is (an immutable) value you can, as a side effect, store it into a location of your choosing and later continue the execution where it left off. When the execution is paused like this, it does not block a thread. It's just a value in the memory.

Here's an example, using the LoggingExecutionService to inspect the execution details:

```scala
import sanoitus._
import sanoitus.util.LoggingExecutionService

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

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
```

The output will be something like:

```stdout
39ms           pool-1-thread-1                Return(5)
54ms           pool-1-thread-1                => 5
63ms           pool-1-thread-1                [effect]
1071ms         pool-1-thread-2                => 8 => 40
Right(40)
```

As seen from the output, a different thread happens to pick up the execution after the artificial pause.

## Resources

Resources represent things that are supposed to be somehow freed, sooner or later. Resources can be defined by pairing an arbitrary value with a function that frees it.

Created resources will be attached to the execution. If an execution ends without an attached resource being closed, the ExecutionService will close it:

```scala
val es = BasicExecutionService()

val program = for {
  res <- resource("resource1")(res => println(s"closing resource $res"))
  _ <- resource("resource2")(res => println(s"closing resource $res"))
  _ <- close(res)
} yield ()

// will print "
//  closing resource resource1
//  closing resource resource2
// "
es.executeUnsafe(program)
```

In a more realistic setting the closing functions could, for example, remove a key/value pair from a map, the key matching the resource value.

## Included languages

This repository contains 3 languages. The main focus above was the root language which contains primitive operations. The other included languages are "parallel language" for parallel executions and "stream language" for defining and processing streams.

### Root language

The root language contains the following primitive operations

* unit - returns a value
* effect - performs a side effect
* mapResources - modify the set of resources attached to the execution
* peekResources - read the set of resources currently attached to the execution

and the following operations derived from the primitive ones:

* resource - creates a resource
* close - closes a resource

### Parallel language

Parallel language defines the following operations:

* Fork - fork an execution
* Await - wait until result of a forked execution is available

Example:

```scala
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
```

Resources can be attached to a forked execution. This means they will be automatically closed when the forked execution ends:

```scala
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
```

Note that a resource can be closed only by the execution to which the resource is attached to. Closing a resource from another execution has no effect.

### Stream language

Stream language defines the following operations:

* ReadStream - read a value from a stream
* ReadAll - read all values from a stream
* Process - process a stream for its side effects

and multiple functions to define and manipulate streams.

Example:

```scala
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
      stream.through(value => effect[Unit] { _ => Some(println(s"value from stream: $value")) })
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
```

## How to use it

All the code in this document can be found under 'example' directory as a stand-alone project. It can be compiled with the following command (executed in 'example' directory):

* Scala 2.13: ./gradlew build
* Scala 2.12: ./gradlew -c settings-2.12.gradle build
