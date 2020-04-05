package sanoitus.parallel

import sanoitus._
import sanoitus.util._

trait ParallelLanguage extends Language { self: Interpreter =>

  type Promise[+A]
  type WritablePromise[A] <: Promise[A]

  implicit val promiseMonad: Monad[Promise]

  object IllegalFork extends Throwable

  case class Fork[+A](program: Program[A], resources: Resource[_]*) extends Operation[Promise[A]]
  case class Await[+A](promise: Promise[A]) extends Operation[A]

  case class CreatePromise[A]() extends Operation[WritablePromise[A]]
  case class FulfillPromise[A](promise: WritablePromise[A], value: Either[Throwable, A]) extends Operation[Unit]

  def Parallelize[A](programs: List[Program[A]]): Program[Promise[List[A]]] = {
    import Monad._
    val forkedPrograms = programs.map(Fork(_): Program[Promise[A]])
    val combinedProgram = forkedPrograms.sequence
    for {
      promises <- combinedProgram
    } yield promises.sequence
  }
}
