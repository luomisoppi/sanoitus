package sanoitus
package test

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers

import sanoitus.util._

trait LanguageSpecification extends AnyPropSpec with Checkers {

  def name: String

  val es: LoggingExecutionService

  type State

  trait ProgramGenerator {
    type Arguments
    type Result
    def precondition(state: State): Boolean
    def apply(state: State): Gen[Program[(Arguments, Result)]]
    def analyze(state: State, arguments: Arguments, result: Result): Program[Either[String, State]]
  }

  val programGenerators: List[ProgramGenerator]

  val genInitialState: Gen[Program[State]]

  def genProgram(state: State, steps: Int): Program[Either[String, State]] = {
    val programGenerator = Gen.oneOf(programGenerators.filter(_.precondition(state))).sample.get
    val program = programGenerator(state).sample.get

    program.flatMap { output =>
      programGenerator.analyze(state, output._1, output._2).flatMap {
        case Right(state) if steps > 1 => genProgram(state, steps - 1)
        case Right(state)              => unit(Right(state))
        case Left(err)                 => unit(Left(err))
      }
    }
  }

  val programs: Gen[Program[Prop]] = Gen.sized { sz =>
    for {
      stateProg <- genInitialState
      prog = for {
        initial <- stateProg
        // always at least 2 steps
        res <- genProgram(initial, sz.max(2))
      } yield res
    } yield prog.map {
      case Left(err) => Prop.falsified :| err
      case Right(_)  => Prop.passed
    }
  }

  val currentExecution = new ThreadLocal[String]
  val executions = atom(Map[String, Log]())

  val property =
    forAll(programs) { prog =>

      val oldId = currentExecution.get
      if (oldId != null) {
        cas(executions) { _ - oldId }
      }
      val execId = java.util.UUID.randomUUID().toString()
      val res = es.execute(prog)
      currentExecution.set(execId)
      cas(executions) { _ + ((execId, res.meta)) }

      res.value match {
        case Right(prop) => prop :| "exec: " + execId
        case Left(_)     => Prop.falsified :| "exec: " + execId
      }
    }

  property(name) {
    try {
      check(property)
    } catch {
      case g: GeneratorDrivenPropertyCheckFailedException => {
        println("--- Error trace ---")
        try {
          val executionId = g.labels.find(_.startsWith("exec: ")).get.substring(6)
          val exec = executions.get.get(executionId).get
          println(exec)
          println("--- Error trace ends ---")
        } catch {
          case t: Throwable => t.printStackTrace()
        }
        g.printStackTrace()
        throw g
      }
      case t: Throwable => {
        t.printStackTrace()
        throw t
      }
    }
  }
}
