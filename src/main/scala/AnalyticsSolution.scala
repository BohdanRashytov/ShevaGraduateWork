object AnalyticsSolution extends Method{
  def calculate: List[Double] = if (rho / n < 1) {
    val p0 = 1.0 / ((0 to n).map(k => Math.pow(rho, k)/(1 to k).product).sum + Math.pow(rho, n+1)/((1 to n).product*(n-rho)))
    val p1_n = (1 to n).map(k => Math.pow(rho, k)*p0/(1 to k).product).toList
    val pN = (n + 1 to 10*n).map(k => Math.pow(rho, k)*p0/((1 to n).product*Math.pow(n, k-n))).toList
    List(p0) ::: p1_n ::: pN
  } else List()
}
