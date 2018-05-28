package shevagraduatework

import java.io.{BufferedWriter, FileWriter}
import GraduateWork._

object Writer {
  def write(name: String, list: List[Double]) = {
    val folder = s"src/main/resources"
    val buffer = new BufferedWriter(new FileWriter(s"$folder/$name.txt"))
    buffer.write(name)
    buffer.write("\n")

    buffer.write("lambda = " + lambda)
    buffer.write("\n")

    buffer.write("mu = " + mu)
    buffer.write("\n")

    buffer.write("rho = " + rho)
    buffer.write("\n")

    buffer.write("channel = " + n)
    buffer.write("\n")
    buffer.write("\n")

    list.zipWithIndex.foreach(pair => {
      buffer.write(s"P${pair._2} = ${pair._1}")
      buffer.write("\n")
    })

    buffer.flush()
  }

}
