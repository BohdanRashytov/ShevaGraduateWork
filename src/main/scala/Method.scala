package shevagraduatework

trait Method {
  val lambda = GraduateWork.lambda
  val mu = GraduateWork.mu
  val rho = GraduateWork.rho
  val n = GraduateWork.n

  def calculate: List[Double]
}
