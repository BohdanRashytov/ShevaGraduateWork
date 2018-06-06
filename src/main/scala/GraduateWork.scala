object GraduateWork {

  val lambda = 0.8
  val mu = 1.2
  val rho = lambda / mu
  val n = 3
  val delta = 1.0
  val U = 5

  case class TimeStates(inFuture: List[Double], inQueue: Int, inProcessing: List[Double])

  def main(args: Array[String]): Unit = {
    var time = System.currentTimeMillis()
    val analyticsSolution = AnalyticsSolution
    val analyticsCalculate = analyticsSolution.calculate
    println(s"Success Analytic (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val monteCarloSolution = MonteCarloSolution
    val monteCarloCalculate = monteCarloSolution.calculate
    println(s"Success Monte Carlo (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val T = (monteCarloSolution.getMaxT / delta).toInt
    val times = (0 to T).map(_ * delta).toList
    val states = monteCarloSolution.getTimeStates(times)
    val fastSimulation = FastSimulation
    fastSimulation.processing(times.drop(1), states.drop(1), monteCarloSolution.getMaxT)
    val fastCalculate = fastSimulation.getPu()
    println(s"Success Fast Simulation (${(System.currentTimeMillis() - time) / 1000.0})")


    Writer.write("AnalyticsSolution", analyticsCalculate, U)
    Writer.write("MonteCarloSolution", monteCarloCalculate, U)
    Writer.writeFS("FastSimulation", fastCalculate, U, delta)

    println()
    println("-----Calculate")
    println("Analytic = " + (1 - analyticsCalculate.take(U).sum))
    println("Monte Carlo = " + (1 - monteCarloCalculate.take(U).sum))
    println("FastSimulation = " + fastCalculate)

  }
}
