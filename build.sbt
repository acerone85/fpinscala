val commonSettings = Seq(
  scalaVersion := "2.12.8",
  libraryDependencies ++=
    Seq(
      "org.scalactic" %% "scalactic" % "3.1.1",
      "org.scalatest" %% "scalatest" % "3.1.1" % "test",
      "joda-time" % "joda-time" % "2.10.5",
      "org.scalacheck" %% "scalacheck" % "1.14.2" % "test",
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "1.0.0-SNAP3"
    )
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(name := "fpinscala")

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )

