import GraduateWork._

object FastSimulation {

  var pi: List[Double]= List()

  def F(delta: Double) = 1 - Math.exp(-lambda*delta)

  def generateHandleTime = (-1) * Math.log(1 - Math.random()) / mu

  def uniform = Math.random()

  def calculateTau(delta: Double) = (-1)*Math.log(1 - F(delta)*uniform) / lambda





  def processing(times: List[Double], states: List[TimeStates], endMax: Double): Unit = times.indices.foreach(i => processing(times(i), if (i == times.size - 1) endMax else times(i+1), states(i)))

  def processing(time: Double, nexTime: Double, timeStates: TimeStates): Unit = {
    var inFuture = timeStates.inFuture
    var inQueue = timeStates.inQueue
    var inProcessing = timeStates.inProcessing
    var currentTime = time
    var p = 1.0

    def requestInSystem: Int = inQueue + inProcessing.size

    def getMinTime = inFuture.headOption.getOrElse(Double.MaxValue) min inProcessing.headOption.getOrElse(Double.MaxValue)

    def nextArrival = inFuture.nonEmpty && getMinTime == inFuture.headOption.getOrElse(Double.MaxValue)

    def nextHandled = inProcessing.nonEmpty && getMinTime == inProcessing.headOption.getOrElse(Double.MaxValue)

    def isFree = inProcessing.size < n

    def existQueue = inQueue > 0

    def flag = inFuture.size + inQueue + inProcessing.size > 0

    def achievedGoal(t: Double) = inProcessing.headOption.getOrElse(Double.MaxValue) > t && inFuture.headOption.getOrElse(Double.MaxValue) > t

    var exitConditions = false

    def goToTime(t: Double) = {
      while (flag && !achievedGoal(t)) {
        if (nextArrival && inFuture.head <= t) {
          val arrival = inFuture.head
          currentTime = arrival
          inFuture = inFuture.tail
          if (isFree) {
            val newHandle = arrival + generateHandleTime
            inProcessing = (newHandle :: inProcessing).sorted
          }
          else inQueue += 1
        } else if (nextHandled && inProcessing.head <= t) {
          val handled = inProcessing.head
          currentTime = handled
          inProcessing = inProcessing.tail
          if (existQueue) {
            inQueue -= 1
            val newHandle = handled + generateHandleTime
            inProcessing = (newHandle :: inProcessing).sorted
          }
        }
      }
    }

    while (!exitConditions){
      (requestInSystem < U, inProcessing.headOption.getOrElse(Double.MaxValue) > nexTime) match {
        case (true, _) =>
          var condition = false
          while (!condition) {
            val tau = calculateTau(nexTime - currentTime)
            inFuture = ((currentTime + tau) :: inFuture).sorted
            p = p * F(nexTime - currentTime)
            goToTime(currentTime + tau)
            condition = requestInSystem >= U && inProcessing.sorted.headOption.getOrElse(Double.MaxValue) > nexTime
          }
          exitConditions = true
        case (_, true) =>
          exitConditions = true
        case (_, _) =>
          val clearHandle = inProcessing.head
          goToTime(clearHandle)
      }
    }

    pi = p :: pi
  }

  def getPu() = pi.sum / pi.size
}
