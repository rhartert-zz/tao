package tao.core

import oscar.cp._
import scala.util.Random
import tao.io.Parameters
import tao.io.Instance
import tao.io.Solution
import scala.collection.mutable.ArrayBuffer



class TAOptimizer(instance: Instance, parameters: Parameters) {

  def this(instance: Instance) = this(instance, new Parameters)

  // Parameters
  private val timeLimit = parameters.timeLimit * 1000
  private val verbous = parameters.verbous
  private val relaxSize = parameters.relaxSize
  private val maxFailsLns = parameters.maxFailsLns
  private val stagnancyIter = parameters.stagnancyIter
  private val stagnancyFail = parameters.stagnancyFail
  private val rand = new Random(parameters.seed)

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
  private val oldAssignments = instance.oldAssignments

  // Main solver
  private implicit val solver = CPSolver()
  private val searchEngine = solver.searchEngine
  solver.silent = true
  
  // Constraints
  private val constraints = ArrayBuffer[Constraint]()

  // Variables 
  private val loads = Array.fill(nAssistants + 1)(CPIntVar(0, totalHours))
  private val deltas = Array.tabulate(nAssistants)(a => loads(a) - maxHours(a))
  private val differences = Array.tabulate(nAssistants)(a => absolute(deltas(a)))
  private val assignments = Array.tabulate(nCourses)(c => CPIntVar(courseAssistants(c)))
  private val maxDifference = maximum(differences)

  // Link assignations to hours
  constraints.append(binPacking(assignments, hours, loads))

  // Required assistants
  for (pair <- required) {
    constraints.append(assignments(pair.course) == pair.assistant)
  }

  // Forbidden assistants
  for (pair <- forbidden) {
    constraints.append(assignments(pair.course) != pair.assistant)
  }

  // Objective to minimize
  minimize(maxDifference)
  
  // Add the constraints
  add(constraints)

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
    if (verbous) {
      val solutionTime = System.currentTimeMillis() - startingTime
      val solNNodes = nNodes + searchEngine.nNodes
      val solNFails = nFails + searchEngine.nBacktracks
      println(s"$bestValue\t$solNNodes\t$solNFails\t$solutionTime\t$nIterations")
    }
    // Reset stagnancy
    maxIterations = nIterations + stagnancyIter
    maxBacktracks = searchEngine.nBacktracks + stagnancyFail
  }

  final def solve(): Solution = {
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
    if (verbous) println("obj.\tnNodes\tnFails\ttime\trestart")
  }

  @inline private def buildSolution: Solution = {
    if (bestValue == Int.MaxValue) { // no solution
      if (verbous) println("no solution")
      null
    } else {
      if (verbous) {
        val time = System.currentTimeMillis - startingTime
        if (opt) println("stop       : optimal solution")
        else if (time >= timeLimit) println("stop       : time limit")
        else println("stop       : stagnancy")
        println(s"time (ms)  : $time")
        println(s"iterations : $nIterations")
        println(s"objective  : $bestValue")
        println(s"optimal    : $opt")
      }
      new Solution(solution.map(a => if (a == nAssistants) -1 else a), solDifferences)
    }
  }

  /** Forget the best solution found. */
  final def resetSolution(): Unit = {
    solver.obj(maxDifference).relax()
  }
}