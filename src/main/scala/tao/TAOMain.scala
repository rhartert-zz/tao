package tao

import scala.util.Random

object TAOMain extends App {

  val rand = new Random(56)

  val nAssistants: Int = 150
  val Assistants = 0 until nAssistants
  val maxLoads: Array[Int] = Array.fill(nAssistants + 1) {
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
    nAssistants,
    maxLoads,
    nCourses,
    requirements,
    possibleAssistants.map(_.toArray),
    Array.empty[(Int, Int)],
    Array.empty[(Int, Int)],
    Array.fill(nCourses)(rand.nextInt(nAssistants))
  )

  val parameters = new TAOParameters(
    timeLimit = 20,
    relaxSize = 30,
    maxFailsLns = 500,
    stagnancyIter = 10,
    stagnancyFail = 500,
    seed = 0,
    recordLog = true
  )

  val solver = new TAOptimizer(instance, parameters)
  val solution = solver.solve()
  
  solver.getLog.foreach(println)

  /*println
  println("SOLUTION")
  println("--------")
  for (i <- 0 until nCourses) {
    print(s"Course_${i + 1} : ")
    val assistant = solution.assignments(i)
    if (assistant == -1) println("empty")
    else println(s"Assistant_${solution.assignments(i) + 1}")
  }*/
}