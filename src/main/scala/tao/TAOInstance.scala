package tao

class TAOInstance(
  val nAssistants: Int,
  val maxHours: Array[Int],
  val nCourses: Int,
  val hours: Array[Int],
  val courseAssistants: Array[Array[Int]],
  val required: Array[(Int, Int)],
  val forbidden: Array[(Int, Int)],
  val oldSolution: Array[Int]
)
