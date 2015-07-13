package tao

import scala.util.Random
import tao.util.Pair

object TAOMain extends App {

  val rand = new Random(56)

  val nAssistants: Int = 150
  val Assistants = 0 until nAssistants
  val maxLoads: Array[Int] = Array.fill(nAssistants) {
    rand.nextInt(4) match {
      case 0 => 50
      case 1 => 100
      case 2 => 100
      case 3 => 200
    }
  }

  val nCourses: Int = 500
  val Courses = 0 until nCourses
  val requirements: Array[Int] = Array.fill(nCourses)(rand.nextInt(3) * 10 + 26 + rand.nextInt(5))

  val possibleAssistants = Array.fill(nCourses)((0 to nAssistants).filter(i => {
    if (i == nAssistants) true
    else rand.nextInt(5) == 0
  }))

  val instance = new TAOInstance(
    maxLoads,
    requirements,
    possibleAssistants.map(_.toArray),
    Array.empty[Pair],
    Array.empty[Pair],
    Array.fill(nCourses)(rand.nextInt(nAssistants))
  )

  val parameters = new TAOParameters(
    timeLimit = 20,
    relaxSize = 30,
    maxFailsLns = 500,
    stagnancyIter = 10,
    stagnancyFail = 500,
    seed = 0,
    verbous = true
  )

  val solver = new TAOptimizer(instance, parameters)
  val solution = solver.solve()
}