package tao

class TAOParameters(
  val timeLimit: Int = 10,
  val relaxSize: Int = 30,
  val maxFailsLns: Int = 500,
  val stagnancyIter: Int = 10,
  val stagnancyFail: Int = 500,
  val seed: Int = 0,
  val verbous: Boolean = true
)