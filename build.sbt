name := "fun_compiler"

version := "0.1"

scalaVersion := "2.13.2"


libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.2.0",
  "org.ow2.asm" % "asm" % "8.0.1",
  "org.ow2.asm" % "asm-tree" % "8.0.1",
  "org.scalatest" %% "scalatest-funsuite" % "3.2.0" % "test",
  "org.scalatest" %% "scalatest-mustmatchers" % "3.2.0" % "test"
)
