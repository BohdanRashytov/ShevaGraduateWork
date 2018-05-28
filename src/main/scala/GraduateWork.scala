package shevagraduatework

object GraduateWork {

  val lambda = 0.8
  val mu = 1.2
  val rho = lambda / mu
  val n = 2

  def main(args: Array[String]): Unit = {
    val analyticsSolution = AnalyticsSolution.calculate
    val monteCarloSolution = MonteCarloSolution.calculate

    Writer.write("AnalyticsSolution", analyticsSolution)
    Writer.write("MonteCarloSolution", monteCarloSolution)

    Graph.paintLines(analyticsSolution, monteCarloSolution)
    Graph.paintDiff(analyticsSolution, monteCarloSolution)
  }
}
