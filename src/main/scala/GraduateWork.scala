object GraduateWork {

  val lambda = 1.0
  val mu = 1.0
  val rho = lambda / mu
  val n = 3
  val delta = 1.0
  val U = 5
  val N = Math.pow(10, 5).toInt

  case class TimeStates(inFuture: List[Double], inQueue: Int, inProcessing: List[Double])

  def main(args: Array[String]): Unit = {
    run
  }

  def run = {
    println("lambda = " + lambda)
    println("mu = " + mu)
    println("rho = " + rho)
    println("channels = " + n)
    println("U = " + U)
    println("N = " + N)
    
    var time = System.currentTimeMillis()
    val analyticsSolution = AnalyticsSolution
    val analyticsCalculate = analyticsSolution.calculate
    println(s"Success Analytic (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val monteCarloSolution = new MonteCarloSolution()
    val monteCarloCalculate = monteCarloSolution.calculate
    val monteCarloStandardDeviation = monteCarloSolution.getStandardDeviation()
    println(s"Success Monte Carlo (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val T = (monteCarloSolution.getMaxT / delta).toInt
    val times = (0 to T).map(_ * delta).toList
    val states = monteCarloSolution.getTimeStates(times)
    println(s"Processing time (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val fastSimulation = new FastSimulation()
    fastSimulation.processing(times, states, monteCarloSolution.getMaxT)
    val fastCalculateWithL = fastSimulation.getPuWithL()
    val fastWithLStandardDeviation = fastSimulation.getStandardDeviationWithL()
    println(s"Success Fast Simulation (${(System.currentTimeMillis() - time) / 1000.0})")

//    Writer.write("AnalyticsSolution", analyticsCalculate, U)
//    Writer.write("MonteCarloSolution", monteCarloCalculate, U)
//    Writer.writeFS("FastSimulation", fastCalculateWithL, U, delta)

    println("-----Calculate")
    println("Analytic = " + (1 - analyticsCalculate.take(U).sum))
    println("Monte Carlo = " + (1 - monteCarloCalculate.take(U).sum))
    println("Fast Simulation = " + fastCalculateWithL)

    println("-----StandardDeviation")
    println("Monte Carlo = " + monteCarloStandardDeviation)
    println("Fast Simulation = " + fastWithLStandardDeviation)
  }
}
