object GraduateWork {

  val lambda = 0.8
  val mu = 1.2
  val rho = lambda / mu
  val n = 3
  val delta = 1.0
  val U = 5

  case class TimeStates(state: Int, startPoints: List[Double], endPoints: List[Double])

  def main(args: Array[String]): Unit = {
    val analyticsSolution = AnalyticsSolution
    val analyticsCalculate = analyticsSolution.calculate
    println("Success Analytic")

    val monteCarloSolution = MonteCarloSolution
    val monteCarloCalculate = monteCarloSolution.calculate
    println("Success Monte Carlo")

    val startPoint = monteCarloSolution.getStartPoints()
    val endPoints = monteCarloSolution.getEndPoints()
    val T = (endPoints.last / delta).toInt + 1
    val times = (1 to T).map(_*delta).toList
    val states = monteCarloSolution.getTimeStates(times)
    val fastSimulation = FastSimulation
    fastSimulation.processing(startPoint, endPoints, times, states)
    val fastCalculate = fastSimulation.getPu()
    val fastStandardDeviation = fastSimulation.getStandardDeviation()
    println("Success Fast Simulation")

    Writer.write("AnalyticsSolution", analyticsCalculate, U)
    Writer.write("MonteCarloSolution", monteCarloCalculate, U)
    Writer.writeFS("FastSimulation", fastCalculate, U, delta)

    println()
    println("-----Calculate")
    println("Analytic = " + (1 - analyticsCalculate.take(U + 1).sum))
    println("Monte Carlo = " + (1 - monteCarloCalculate.take(U + 1).sum))
    println("FastSimulation = " + fastCalculate)

    println()
    println("-----StandardDeviation")
    println("Fast Simulation  = " + fastStandardDeviation)

//    Graph.paintLines(analyticsCalculate, monteCarloCalculate)
//    Graph.paintDiff(analyticsCalculate, monteCarloCalculate)
  }
}
