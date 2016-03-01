name := "picc"

version := "1.0"

scalaVersion := "2.10.5" //"2.11.7" // "2.10.5" --> needed for z3

libraryDependencies ++= Seq(
  // unit tests
  "org.scalatest" % "scalatest_2.10" % "2.2.6" % "test",
  "junit" % "junit" % "4.12",
  // choco constraint solver
  "choco" % "choco" % "2.1.5" from
    "http://downloads.sourceforge.net/project/choco/choco/2.1.5/choco-2.1.5/choco-solver-2.1.5.jar"
)    