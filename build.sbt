name := "ShevaGraduateWork"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.6"

lazy val root = (project in file("."))
  .settings(name := "root")
  .settings(
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathIO" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathArray" % "1.0"
  )
        