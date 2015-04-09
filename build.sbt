name := "ScalaFormalCROM"

scalaVersion := "2.11.6"

val scalatestV = "2.2.1"

version := "0.0.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalatestV % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false