name := "damo-core"

organization := "tk.spaceli"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq (
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
)