import akka._

scalaVersion := "2.11.8"
libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "1.3.0",
  "com.lihaoyi" %% "scalaparse" % "0.4.2",
  Dependencies.Compile.Test.scalatest.value
)
