lazy val root = (project in file(".")).
  settings(
    name := "OReillyLearningScala",
    version := "1.0",
    scalaVersion := "2.11.7",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.3.0"
  )


