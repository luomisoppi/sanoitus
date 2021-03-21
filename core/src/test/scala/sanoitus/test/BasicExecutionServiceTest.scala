package sanoitus.test

import sanoitus._

class BasicExecutionServiceTest extends ExecutionServiceTest {
  val es = BasicExecutionService(10, 1000, this.sink)
}
