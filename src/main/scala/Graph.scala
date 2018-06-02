import java.awt.{Color, Frame}
import javax.swing.JFrame

import org.math.plot.Plot2DPanel

object Graph {

  def paintLines(analyticsSolution: List[Double] = List(), monteCarloSolution: List[Double] = List(), plotName: String = "Graph") = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    frame.setContentPane(plot)

    plot.addLinePlot(s"$plotName-Analytics points", Color.GREEN, analyticsSolution.indices.map(_.toDouble).toArray, analyticsSolution.toArray)
    plot.addBarPlot(s"$plotName-Analytics points", Color.GREEN, analyticsSolution.indices.map(_.toDouble).toArray, analyticsSolution.toArray)

    plot.addLinePlot(s"$plotName-Analytics points", Color.BLUE, monteCarloSolution.indices.map(_.toDouble).toArray, monteCarloSolution.toArray)
    plot.addBarPlot(s"$plotName-Analytics points", Color.BLUE, monteCarloSolution.indices.map(_.toDouble).toArray, monteCarloSolution.toArray)
  }

  def paintDiff(analyticsSolution: List[Double] = List(), monteCarloSolution: List[Double] = List(), plotName: String = "Error") = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    frame.setContentPane(plot)

    val list = (1 until (monteCarloSolution.size min analyticsSolution.size)).map(i => Math.abs(analyticsSolution(i) - monteCarloSolution(i)))

    plot.addLinePlot(s"$plotName-Analytics points", Color.RED, list.indices.map(_.toDouble).toArray, list.toArray)
    plot.addBarPlot(s"$plotName-Analytics points", Color.RED, list.indices.map(_.toDouble).toArray, list.toArray)
  }
}