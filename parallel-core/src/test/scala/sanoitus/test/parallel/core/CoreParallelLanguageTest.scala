package sanoitus.test.parallel.core

import sanoitus._
import sanoitus.parallel.core._
import sanoitus.test.parallel._

class CoreParallelLanguageTest extends ParallelLanguageTest {
  override val es = BasicExecutionService(10, 10, this.sink)
  override val language = ParallelInterpreter
}
