object GraduateWork {

  val lambda = 0.6
  val mu = 0.6
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
    val monteCarloStandardDeviation = monteCarloSolution.getStandardDeviation()
    println(s"Success Monte Carlo (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val T = (monteCarloSolution.getMaxT / delta).toInt
    val times = (0 to T).map(_ * delta).toList
    val states = monteCarloSolution.getTimeStates(times)
    println(s"Processing time (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val fastSimulationWithoutGen = FastSimulationWithoutGen
    fastSimulationWithoutGen.processing(times, states, monteCarloSolution.getMaxT)
    val fastCalculateWithoutGen = fastSimulationWithoutGen.getPu()
    val fastCalculateWithoutGenWithL = fastSimulationWithoutGen.getPuWithL()
    val fastWithoutGenStandardDeviation = fastSimulationWithoutGen.getStandardDeviation()
    val fastWithoutGenWithLStandardDeviation = fastSimulationWithoutGen.getStandardDeviationWithL()
    println(s"Success Fast Simulation Without Generation (${(System.currentTimeMillis() - time) / 1000.0})")

    time = System.currentTimeMillis()
    val fastSimulationWithGen = FastSimulationWithGen
    fastSimulationWithGen.processing(times, states, monteCarloSolution.getMaxT)
    val fastCalculateWithGen = fastSimulationWithGen.getPu()
    val fastCalculateWithGenWithL = fastSimulationWithGen.getPuWithL()
    val fastWithGenStandardDeviation = fastSimulationWithGen.getStandardDeviation()
    val fastWithGenWithLStandardDeviation = fastSimulationWithGen.getStandardDeviationWithL()
    println(s"Success Fast Simulation With Generation (${(System.currentTimeMillis() - time) / 1000.0})")


    Writer.write("AnalyticsSolution", analyticsCalculate, U)
    Writer.write("MonteCarloSolution", monteCarloCalculate, U)
    Writer.writeFS("FastSimulationWithoutGen", fastCalculateWithoutGen, U, delta)
    Writer.writeFS("FastSimulationWithoutGenWithL", fastCalculateWithoutGenWithL, U, delta)
    Writer.writeFS("FastSimulationWithGen", fastCalculateWithGen, U, delta)
    Writer.writeFS("FastSimulationWithGenWithL", fastCalculateWithGenWithL, U, delta)

    println()
    println("-----Calculate")
    println("Analytic = " + (1 - analyticsCalculate.take(U).sum))
    println("Monte Carlo = " + (1 - monteCarloCalculate.take(U).sum))
    println("Fast Simulation Without Gen = " + fastCalculateWithoutGen)
    println("Fast Simulation Without Gen With L = " + fastCalculateWithoutGenWithL)
    println("Fast Simulation With Gen = " + fastCalculateWithGen)
    println("Fast Simulation With Gen With L = " + fastCalculateWithGenWithL)

    println()
    println("-----StandardDeviation")
    println("Monte Carlo = " + monteCarloStandardDeviation)
    println("Fast Simulation Without Gen = " + fastWithoutGenStandardDeviation)
    println("Fast Simulation Without Gen With L = " + fastWithoutGenWithLStandardDeviation)
    println("Fast Simulation With Gen = " + fastWithGenStandardDeviation)
    println("Fast Simulation With Gen With L = " + fastWithGenWithLStandardDeviation)

  }
}
