package tao.core

import oscar.cp.core.variables.CPIntVar
import oscar.algo.search._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.searches.Decision
import scala.util.Random

final class TAOHeuristic(assignments: Array[CPIntVar], differences: Array[CPIntVar], oldAssignments: Array[Int], rand: Random) extends Branching {

  require(assignments.length > 0)

  private[this] val nVariables = assignments.length
  private[this] val emptyAssistant = differences.length
  private[this] val store = assignments(0).store

  private[this] val assistants = new Array[Int](maxAssistants)

  private[this] val unassigned = Array.tabulate(nVariables)(i => i)
  private[this] val nUnassignedRev = new ReversibleInt(store, nVariables)
  private[this] var nUnassigned = nVariables

  final override def alternatives: Seq[Alternative] = {
    updateUnassigned()
    if (nUnassigned == 0) noAlternative
    else {
      nUnassignedRev.value = nUnassigned // trail
      val varId = selectVariable()
      val value = selectValue(varId)
      val variable = assignments(varId)
      List(Decision.assign(variable, value), Decision.remove(variable, value))
    }
  }

  @inline private def selectVariable(): Int = {
    var i = nUnassigned
    var minId = -1
    var minSize = Int.MaxValue
    var nMin = 0
    while (i > 0) {
      i -= 1
      val varId = unassigned(i)
      val variable = assignments(varId)
      val size = variable.size
      if (size < minSize) {
        nMin = 1
        minSize = size
        minId = varId
      } else if (size == minSize) {
        nMin += 1
        if (rand.nextInt(nMin) == 0) minId = varId
      }
    }
    minId
  }

  @inline private def selectValue(varId: Int): Int = {
    val courseVar = assignments(varId)
    val oldAssistant = oldAssignments(varId)
    if (courseVar.hasValue(oldAssistant) && differences(oldAssistant).min < 0) oldAssistant
    else {
      var i = courseVar.fillArray(assistants)
      var value = -1
      var minDiff = Int.MaxValue
      while (i > 0) {
        i -= 1
        val assistant = assistants(i)
        val lowerDiff = if (assistant == emptyAssistant) Int.MaxValue
        else differences(assistant).min
        if (lowerDiff < minDiff) {
          value = assistant
          minDiff = lowerDiff
        }
      }
      value
    }
  }

  @inline private def updateUnassigned(): Unit = {
    nUnassigned = nUnassignedRev.value // cache
    var i = nUnassigned
    while (i > 0) {
      i -= 1
      val varId = unassigned(i)
      val variable = assignments(varId)
      if (variable.isBound) {
        nUnassigned -= 1
        unassigned(i) = unassigned(nUnassigned)
        unassigned(nUnassigned) = varId
      }
    }
  }

  @inline private def maxAssistants: Int = {
    var i = nVariables
    var max = Int.MinValue
    while (i > 0) {
      i -= 1
      val size = assignments(i).size
      if (size > max) max = size
    }
    max
  }

}