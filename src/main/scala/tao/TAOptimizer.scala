package tao

import oscar.cp._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import oscar.cp.searches.ConflictOrderingSearch

class TAOParameters(
  val timeLimit: Int = 10,
  val relaxSize: Int = 30,
  val maxFailsLns: Int = 500,
  val stagnancyIter: Int = 10,
  val stagnancyFail: Int = 500,
  val seed: Int = 0,
  val recordLog: Boolean = true
)

class TAOptimizer(instance: TAOInstance, parameters: TAOParameters) {

  def this(instance: TAOInstance) = this(instance, new TAOParameters)

  // Parameters
  private val timeLimit = parameters.timeLimit * 1000
  private val recordLog = parameters.recordLog
  private val relaxSize = parameters.relaxSize
  private val maxFailsLns = parameters.maxFailsLns
  private val stagnancyIter = parameters.stagnancyIter
  private val stagnancyFail = parameters.stagnancyFail
  private val rand = new Random(parameters.seed)

  // Log
  private val logContent: ArrayBuffer[String] = ArrayBuffer()

  // Courses
  private val nCourses = instance.nCourses
  private val hours = instance.hours
  private val courseAssistants = instance.courseAssistants
  private val totalHours = hours.sum

  // Assistants
  private val nAssistants = instance.nAssistants
  private val maxHours = instance.maxHours

  // User constraints
  private val required = instance.required
  private val forbidden = instance.forbidden
  private val oldAssignments = instance.oldSolution

  // Main solver
  private implicit val solver = CPSolver()
  private val searchEngine = solver.searchEngine
  solver.silent = true

  // Variables 
  private val loads = Array.fill(nAssistants + 1)(CPIntVar(0, totalHours))
  private val deltas = Array.tabulate(nAssistants)(a => loads(a) - maxHours(a))
  private val differences = Array.tabulate(nAssistants)(a => absolute(deltas(a)))
  private val assignments = Array.tabulate(nCourses)(c => CPIntVar(courseAssistants(c)))
  private val maxDifference = maximum(differences)

  // Link assignations to hours
  add(binPacking(assignments, hours, loads))

  // Required assistants
  for ((course, assistant) <- required) {
    add(assignments(course) == assistant)
  }

  // Forbidden assistants
  for ((course, assistant) <- forbidden) {
    add(assignments(course) != assistant)
  }

  // Objective to minimize
  minimize(maxDifference)

  // Search procedure
  search(new TAOHeuristic(assignments, deltas, oldAssignments, rand))

  // Best solution found
  private val solution = new Array[Int](nCourses)
  private val solDifferences = new Array[Int](nAssistants)
  private var bestValue = Int.MaxValue

  // Search state
  private var nNodes: Int = 0
  private var nFails: Int = 0
  private var startingTime: Long = 0
  private var endingTime: Long = 0
  private var nIterations = 0
  private var maxIterations = 0
  private var maxBacktracks = 0
  private var opt = false

  solver.onSolution {
    bestValue = maxDifference.value
    // Copy solution
    var i = nCourses
    while (i > 0) { i -= 1; solution(i) = assignments(i).value }
    i = nAssistants
    while (i > 0) { i -= 1; solDifferences(i) = differences(i).value }
    // Log info
    if (recordLog) {
      val solutionTime = System.currentTimeMillis() - startingTime
      val solNNodes = nNodes + searchEngine.nNodes
      val solNFails = nFails + searchEngine.nBacktracks
      log(s"$bestValue\t$solNNodes\t$solNFails\t$solutionTime\t$nIterations")
    }
    // Reset stagnancy
    maxIterations = nIterations + stagnancyIter
    maxBacktracks = searchEngine.nBacktracks + stagnancyFail
  }

  final def solve(): TAOSolution = {
    // Initialize search states
    initSearch()
    // First solution
    val stats = solver.start {
      (searchEngine.nSolutions >= 1 &&
        searchEngine.nBacktracks >= maxBacktracks) ||
        System.currentTimeMillis >= endingTime
    }
    nNodes += stats.nNodes
    nFails += stats.nFails
    opt = stats.completed
    // Start LNS
    var stop = opt || System.currentTimeMillis >= endingTime
    while (!stop) {
      nIterations += 1
      solver.pushState()
      relaxSolution()
      val stats = solver.start {
        System.currentTimeMillis >= endingTime ||
          searchEngine.nBacktracks >= maxFailsLns
      }
      solver.pop()
      nNodes += stats.nNodes
      nFails += stats.nFails
      stop = opt || nIterations > maxIterations || System.currentTimeMillis >= endingTime
    }
    // Build the solution
    buildSolution
  }

  @inline private def relaxSolution(): Unit = {
    var i = nCourses
    while (i > 0) {
      i -= 1
      val assistant = solution(i)
      if (assistant != nAssistants && solDifferences(assistant) < bestValue && rand.nextInt(100) < relaxSize) {
        add(assignments(i) == solution(i))
      }
    }
  }

  @inline private def initSearch(): Unit = {
    nNodes = 0
    nFails = 0
    startingTime = System.currentTimeMillis()
    endingTime = startingTime + timeLimit
    nIterations = 0
    maxIterations = stagnancyIter
    maxBacktracks = stagnancyFail
    opt = false
    if (recordLog) log("obj.\tnNodes\tnFails\ttime\trestart")
  }

  @inline private def buildSolution: TAOSolution = {
    if (bestValue == Int.MaxValue) { // no solution
      if (recordLog) log("no solution")
      null
    } else {
      if (recordLog) {
        val time = System.currentTimeMillis - startingTime
        if (opt) log("stop       : optimal solution")
        else if (time >= timeLimit) log("stop       : time limit")
        else log("stop       : stagnancy")
        log(s"time (ms)  : $time")
        log(s"iterations : $nIterations")
        log(s"objective  : $bestValue")
        log(s"optimal    : $opt")
      }
      new TAOSolution(solution.map(a => if (a == nAssistants) -1 else a), solDifferences)
    }
  }

  /** Forget the best solution found. */
  final def resetSolution(): Unit = {
    solver.obj(maxDifference).relax()
  }

  /** Returns the content of the log. */
  final def getLog: Iterator[String] = logContent.iterator

  // Record the message in the log.
  @inline private def log(msg: String): Unit = println(msg) //logContent.append(s"$msg")
  @inline private def log(): Unit = log("")
}