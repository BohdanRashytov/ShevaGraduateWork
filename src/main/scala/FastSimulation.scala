import GraduateWork._

object FastSimulation {
  var pi: List[Double]= List()

  def processing(startPoints: List[Double],
                 endPoints: List[Double],
                 times: List[Double],
                 timeStates: List[TimeStates]) = {

    def F(x: Double) = 1 - Math.exp(-lambda*x)

    def handlingTime() =  (-1)*Math.log(1 -  Math.random())/mu


    pi = times.indices.map(i => {
      var time = times(i)
      var state = timeStates(i).state
      var ends = timeStates(i).endPoints
      var starts = timeStates(i).startPoints

      var condition = false

      var delta = 1.0
      var tau = 1.0
      var p = 1.0

      while(!condition && tau > Math.pow(10, -50)){
        tau = - Math.log(1-F(delta)*Math.random()) / lambda
        p = p*F(delta)
        delta = delta - tau

        val newStart = time + tau
        time = newStart
        val handleTime = handlingTime()
        val newEnd = state - n match {
          case s if s < 0 => newStart + handleTime
          case s => endPoints.take(s + 1).last + handleTime
        }

        starts = (newStart :: starts).sorted
        ends = (newEnd :: ends).sorted
        val (input, output) = (starts.count(_ < newStart), ends.count(_ < newStart))
        state = input - output

        condition = (state >= U) && ((Double.MaxValue :: ends.map(_ - time).filter(_ > 0)).min > delta)
      }

      p
    }).toList

  }

  def getPu() = pi.sum * 1.0 / pi.size

  def getStandardDeviation() = {
    val p = getPu()
    pi.map(i => (i- p)*(i-p)).sum/pi.size
  }

}
