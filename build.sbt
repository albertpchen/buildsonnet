val scala3Version = "3.0.1"

// project for JVM build (default)
lazy val parser = project
  .in(file("modules/parser"))
  .settings(
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.4",
      "org.scalameta" %%% "munit" % "0.7.26" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
  )
