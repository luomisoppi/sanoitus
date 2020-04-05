package sanoitus.test.stream.core

import sanoitus._
import sanoitus.stream.core._
import sanoitus.test._
import sanoitus.test.stream._

class CoreStreamLanguageTest extends StreamLanguageTest {
  override val es = BasicExecutionService(10, 10, new RamAnomalySink())
  override val language = StreamInterpreter
}
