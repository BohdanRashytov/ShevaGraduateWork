object MonteCarloSolution extends Method {

  val N = 2 * Math.pow(10, 5).toInt

  var inFuture: List[Double]  = {
    def nextElement = (-1)*Math.log(1 -  Math.random())/lambda
    var sum = 0.0
    (1 to N).map(_ => {
      sum += nextElement
      sum
    }).toList
  }
  var inQueue: List[Double] = List()
  var inProcessing: List[Double] = List()

  def getMinTime = inFuture.headOption.getOrElse(Double.MaxValue) min inProcessing.headOption.getOrElse(Double.MaxValue)

  def nextArrival = inFuture.nonEmpty && getMinTime == inFuture.headOption.getOrElse(Double.MaxValue)

  def nextHandled = inProcessing.nonEmpty && getMinTime == inProcessing.headOption.getOrElse(Double.MaxValue)

  def isFree = inProcessing.size < n

  def flag = inFuture.size + inQueue.size + inProcessing.size > 0

  var stateTime = Map[Int, Double]().withDefaultValue(0.0)
  def requestInSystem: Int = inQueue.size + inProcessing.size
  var currentTime: Double = 0.0

  def calState(timeId: Double) = {
    val diffTime = timeId - currentTime
    currentTime = timeId
    stateTime = stateTime + (requestInSystem -> (stateTime(requestInSystem) + diffTime))
  }

  def processing = while(flag){
    if (nextArrival) {
      val arrival = inFuture.head
      calState(arrival)
      inFuture = inFuture.tail
      if (isFree) inProcessing = ((arrival + generateHandleTime) :: inProcessing).sorted
      else inQueue = inQueue ::: List(arrival)
    } else if (nextHandled) {
      val handled = inProcessing.head
      calState(handled)
      inProcessing = inProcessing.tail
      if (inQueue.nonEmpty) {
        inQueue = inQueue.tail
        inProcessing = ((handled + generateHandleTime) :: inProcessing).sorted
      }
    }

//    println("--------------------------------------")
//    println("inFuture = " + inFuture)
//    println("inQueue = " + inQueue)
//    println("inProcessing = " + inProcessing)
//    println("state = " + requestInSystem)
//    println("time = " + currentTime)
//    println("states = " + stateTime)
  }

  def generateHandleTime = (-1)*Math.log(1 -  Math.random())/mu

  def calculate: List[Double] = {
    val t = System.currentTimeMillis()
    processing
    val sum = stateTime.values.sum
    val max = stateTime.keys.max
    println((System.currentTimeMillis() - t) / 1000)
    (0 to max).map(k => stateTime(k) / sum).toList

  }

//  def getStartPoints() = startPoints
//
//  def getEndPoints() = endPoints
//
//  def getTimeStates(times: List[Double]): List[TimeStates] = {
//    times.map(time => {
//      val input = startPoints.count(_ < time)
//      val output = endPoints.count(_ < time)
//      val state = input - output
//      val ePoints = endPoints.filter(_ >= time).take(state max n)
//      val sPoints = startPoints.drop(output).take(state max n)
//      TimeStates(state, sPoints, ePoints)
//    })
//  }


}
