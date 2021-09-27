val scala3Version = "3.0.2"

// project for JVM build (default)
lazy val parser = project
  .in(file("modules/parser"))
  .enablePlugins(NativeImagePlugin)
  .settings(
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "shapeless3-deriving" % "3.0.3",
      "org.typelevel" %% "cats-parse" % "0.3.4",
      "org.scalameta" %%% "munit" % "0.7.26" % Test,
      ("io.get-coursier" %% "coursier" % "2.0.16").cross(CrossVersion.for3Use2_13),
    ),
    nativeImageOptions ++= List(
      "-H:+ReportExceptionStackTraces",
      "--verbose",
      "-H:TempDirectory=~/projects/scala3/build/target/native-image"
    ),
    testFrameworks += new TestFramework("munit.Framework"),
  )
