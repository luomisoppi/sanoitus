package sanoitus.util.test

import sanoitus.util._
import sanoitus.test._

class LoggingExecutionServiceTest extends ExecutionServiceTest {
  val es = LoggingExecutionService(10, 1000, this.sink)
}
