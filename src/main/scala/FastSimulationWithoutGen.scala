import GraduateWork._

object FastSimulationWithoutGen {

  var pi: List[Double]= List()
  var alphaI: List[Double]= List()

  def F(delta: Double) = 1 - Math.exp(-lambda*delta)

  def generateHandleTime = (-1) * Math.log(1 - Math.random()) / mu

  def uniform = Math.random()

  def calculateTau(delta: Double) = (-1)*Math.log(1 - F(delta)*uniform) / lambda

  def processing(times: List[Double], states: List[TimeStates], endMax: Double): Unit = times.indices.foreach(i =>
    processing(times(i), if (i == times.size - 1) endMax else times(i+1), states(i)))

  def processing(time: Double, nextTime: Double, timeStates: TimeStates): Unit = {
    var inFuture = timeStates.inFuture
    var inQueue = timeStates.inQueue
    var inProcessing = timeStates.inProcessing
    var currentTime = time
    var p = 1.0
    var alpha = 0.0

    def requestInSystem: Int = inQueue + inProcessing.size

    def getMinTime = inFuture.headOption.getOrElse(Double.MaxValue) min inProcessing.headOption.getOrElse(Double.MaxValue)

    def nextArrival = inFuture.nonEmpty && getMinTime == inFuture.head

    def nextHandled = inProcessing.nonEmpty && getMinTime == inProcessing.head

    def isFree = inProcessing.size < n

    def existQueue = inQueue > 0

    def achievedGoal(t: Double) = inProcessing.headOption.getOrElse(Double.MaxValue) > t && inFuture.headOption.getOrElse(Double.MaxValue) > t

    var exitConditions = false

    def goToTime(t: Double) = {
      while (!achievedGoal(t)) {
        if (nextArrival) {
          val arrival = inFuture.head
          if (requestInSystem >= U) alpha += arrival - currentTime
          currentTime = arrival
          inFuture = inFuture.tail
          if (isFree) {
            val newHandle = arrival + generateHandleTime
            inProcessing = (newHandle :: inProcessing).sorted
          }
          else inQueue += 1
        } else if (nextHandled) {
          val handled = inProcessing.head
          if (requestInSystem >= U) alpha += handled - currentTime
          currentTime = handled
          inProcessing = inProcessing.tail
          if (existQueue) {
            inQueue -= 1
            val newHandle = handled + generateHandleTime
            inProcessing = (newHandle :: inProcessing).sorted
          }
        }
      }
      if (requestInSystem >= U) alpha += t - currentTime
      currentTime = t
    }

    while (!exitConditions){
      (requestInSystem < U, inProcessing.headOption.getOrElse(Double.MaxValue) > nextTime) match {
        case (true, _) =>
          var condition = false
          while (!condition) {
            val tau = calculateTau(nextTime - currentTime)
            inFuture = ((currentTime + tau) :: inFuture).sorted
            p = p * F(nextTime - currentTime)
            goToTime(currentTime + tau)
            condition = requestInSystem >= U && inProcessing.sorted.headOption.getOrElse(Double.MaxValue) > nextTime
          }
          exitConditions = true
          if (requestInSystem >= U) alpha += nextTime - currentTime
        case (_, true) =>
          exitConditions = true
          if (requestInSystem >= U) alpha += nextTime - currentTime
        case (_, _) =>
          val clearHandle = inProcessing.min
          goToTime(clearHandle)
      }
    }

    pi = p :: pi
    alphaI = alpha :: alphaI
  }

  def getPu() = pi.sum / pi.size

  def getPuWithL() = pi.indices.map(i => pi(i)*alphaI(i)).sum / pi.size

  def getStandardDeviation() = {
    val p = getPu()
    pi.map(i => (i- p)*(i-p)).sum/(pi.size - 1)
  }

  def getStandardDeviationWithL() = {
    val p = getPuWithL()
    pi.indices.map(i => (pi(i)*alphaI(i)- p)*(pi(i)*alphaI(i)-p)).sum/(pi.size - 1)
  }
}
