import GraduateWork._

class FastSimulation() {

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
      requestInSystem < U match {
        case true =>
          var condition = false
          while (!condition) {
            val tau = calculateTau(nextTime - currentTime)
            inFuture = ((currentTime + tau) :: inFuture).sorted
            p = p * F(nextTime - currentTime)
            goToTime(currentTime + tau)
            condition = requestInSystem >= U
          }
          exitConditions = true
          goToTime(nextTime)
        case false =>
          exitConditions = true
          goToTime(nextTime)
      }
    }

    pi = p :: pi
    alphaI = alpha :: alphaI
  }

  def getPuWithL() = pi.indices.map(i => pi(i)*alphaI(i)).sum / pi.size

  def getStandardDeviationWithL() = {
    val p = getPuWithL()
    pi.indices.map(i => (pi(i)*alphaI(i)- p)*(pi(i)*alphaI(i)-p)).sum/(pi.size - 1)
  }
}
