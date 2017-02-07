name := "picc"

version := "1.2"

scalaVersion := "2.11.7" // "2.10.5" --> needed for z3

libraryDependencies ++= Seq(
  // unit tests
  // "org.scalatest" % "scalatest_2.10" % "2.2.6" % "test",
  "junit" % "junit" % "4.12"
  // choco constraint solver // STOPPED working - including jar in the lib folder now.
  // "choco" % "choco" % "2.1.5" from
  //   "https://downloads.sourceforge.net/project/choco/choco/2.1.5/choco-2.1.5/choco-solver-2.1.5.jar?r=&ts=1486465260&use_mirror=netix"
  )