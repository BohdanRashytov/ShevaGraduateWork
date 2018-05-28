package shevagraduatework

object MonteCarloSolution extends Method {

  sealed trait Action
  case object Arrival extends Action
  case object Handled extends Action

  case class TimePoint(timeId: Double, action: Action, priority: Int = 0)


  val N = 10000
  val inputStream = (1 to N).map(_ => (-1)*Math.log(1 -  Math.random())/lambda)
  val inputTimePoint = (1 to N).map(i => TimePoint(inputStream.take(i).sum, Arrival, i))

  var timesPoint = inputTimePoint.toList
  var freeChannels = n

  var stateTime = Map[Int, Double]().withDefaultValue(0.0)
  var currentTime = 0.0
  var requestInSystem = 0

  def processing = {
    while (timesPoint.nonEmpty) {
      val action = timesPoint.filter(_.timeId == timesPoint.head.timeId).minBy(_.priority)

      val diffTime = action.timeId - currentTime
      currentTime = action.timeId
      stateTime = stateTime + (requestInSystem -> (stateTime(requestInSystem) + diffTime))

      action.action match {
        case Handled =>
          timesPoint = timesPoint.drop(1)
          freeChannels += 1
          requestInSystem -= 1
        case Arrival if freeChannels > 0 =>
          timesPoint = timesPoint.drop(1)
          freeChannels -= 1
          timesPoint = (timesPoint ::: List(TimePoint(action.timeId + generateHandleTime, Handled))).sortBy(_.timeId)
          requestInSystem += 1
        case Arrival =>
          val nextHandler = timesPoint.find(_.action == Handled).map(_.timeId).get
          timesPoint = timesPoint.drop(1)
          timesPoint = (timesPoint ::: List(TimePoint(nextHandler, Handled))).sortBy(_.timeId)
          requestInSystem += 1
      }
    }
  }

  def generateHandleTime = (-1)*Math.log(1 -  Math.random())/mu

  def add(list: List[TimePoint], timePoint: TimePoint): List[TimePoint] = {
    List()
  }

  def calculate: List[Double] = {
    processing
    val sum = stateTime.values.sum
    val max = stateTime.keys.max
    (0 to max).map(k => stateTime(k) / sum).toList
  }
}
