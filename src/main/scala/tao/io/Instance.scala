package tao.io

import tao.util.Pair

class Instance(
  val maxHours: Array[Int],                // Maximum number of working hours for assistant i
  val hours: Array[Int],                   // Working hours required by slot i
  val setups: Array[Int],                  // Additional working hours required when starting course i
  val courses: Array[Int],                 // Course id of each slot i
  val courseAssistants: Array[Array[Int]], // Assistants to be assigned to a given a course
  val required: Array[Pair],               // Assistants required to be assigned to a given slot
  val forbidden: Array[Pair],              // Assistants that cannot be assigned to a given slot
  val preferences: Array[Array[Int]],      // Course preferences of each assistant (decreasing order)
  val oldAssignments: Array[Int]           // Old slot assignments
) {
  val nAssistants = maxHours.length
  val nCourses = courseAssistants.length
  val nSlots = courses.length
}