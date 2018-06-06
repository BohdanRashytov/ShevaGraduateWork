import GraduateWork.TimeStates

object MonteCarloSolution extends Method {

  val N = Math.pow(10, 3).toInt

  var inFuture: List[Double] = {
    def nextElement = (-1) * Math.log(1 - Math.random()) / lambda

    var sum = 0.0
    (1 to N).map(_ => {
      sum += nextElement
      sum
    }).toList
  }
  var inQueue: Int = 0
  var inProcessing: List[Double] = List()

  val startPoints: List[Double] = inFuture
  var endPoints: List[Double] = List()

  def getMinTime = inFuture.headOption.getOrElse(Double.MaxValue) min inProcessing.headOption.getOrElse(Double.MaxValue)

  def nextArrival = inFuture.nonEmpty && getMinTime == inFuture.headOption.getOrElse(Double.MaxValue)

  def nextHandled = inProcessing.nonEmpty && getMinTime == inProcessing.headOption.getOrElse(Double.MaxValue)

  def isFree = inProcessing.size < n

  def existQueue = inQueue > 0

  def flag = inFuture.size + inQueue + inProcessing.size > 0

  var stateTime = Map[Int, Double]().withDefaultValue(0.0)

  def requestInSystem: Int = inQueue + inProcessing.size

  var currentTime: Double = 0.0

  def calState(timeId: Double) = {
    val diffTime = timeId - currentTime
    currentTime = timeId
    stateTime = stateTime + (requestInSystem -> (stateTime(requestInSystem) + diffTime))
  }

  def processing = {
    while (flag) {
      if (nextArrival) {
        val arrival = inFuture.head
        calState(arrival)
        inFuture = inFuture.tail
        if (isFree) {
          val newHandle = arrival + generateHandleTime
          endPoints = newHandle :: endPoints
          inProcessing = (newHandle :: inProcessing).sorted
        }
        else inQueue += 1
      } else if (nextHandled) {
        val handled = inProcessing.head
        calState(handled)
        inProcessing = inProcessing.tail
        if (existQueue) {
          inQueue -= 1
          val newHandle = handled + generateHandleTime
          endPoints = newHandle :: endPoints
          inProcessing = (newHandle :: inProcessing).sorted
        }
      }
    }
    endPoints = endPoints.sorted
  }

  def generateHandleTime = (-1) * Math.log(1 - Math.random()) / mu

  def calculate: List[Double] = {
    processing
    val sum = stateTime.values.sum
    val max = stateTime.keys.max
    (0 to max).map(k => stateTime(k) / sum).toList
  }

  def getMaxT = endPoints.last

  def getTimeStates(times: List[Double]): List[TimeStates] = {
    {
      val time = 0.0
      val nextTime = times(1)

      val inFuture = startPoints.filter(p => p > time && p < nextTime)

      val input = startPoints.count(_ < time)
      val output = endPoints.count(_ < time)
      val inQueue = (input - output - n) max 0

      val inProcessing = endPoints.filter(_ > time).take((input - output) min n)

      TimeStates(inFuture, inQueue, inProcessing)
    } ::
    times.indices.map(i => {
      val time = times(i)
      val nextTime = if (i == times.size - 1) Double.MaxValue else times(i+1)

      val inFuture = startPoints.filter(p => p > time && p < nextTime)

      val input = startPoints.count(_ < time)
      val output = endPoints.count(_ < time)
      val inQueue = (input - output - n) max 0

      val inProcessing = endPoints.filter(_ > time).take((input - output) min n)

      TimeStates(inFuture, inQueue, inProcessing)
    }).toList
  }


}
