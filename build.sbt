name := "ScalaFormalCROM"

scalaVersion := "2.12.3"

val scalatestV = "3.0.1"

version := "0.0.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalatestV % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false
