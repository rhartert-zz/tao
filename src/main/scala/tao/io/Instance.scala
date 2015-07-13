package tao.io

import tao.util.Pair

class Instance(
  val maxHours: Array[Int],
  val hours: Array[Int],
  val courseAssistants: Array[Array[Int]],
  val required: Array[Pair],
  val forbidden: Array[Pair],
  val oldAssignments: Array[Int]
) {
  val nAssistants = maxHours.length
  val nCourses = courseAssistants.length
}