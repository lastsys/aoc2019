lazy val aoc2019 = (project in file("."))
  .settings(
    name := "aoc2019",
    version := "0.1",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.1.3",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.2" % Test
    )
  )
