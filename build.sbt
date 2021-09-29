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
      // "-H:+ReportExceptionStackTraces",
      // "--verbose",
      "-H:TempDirectory=~/projects/scala3/build/target/native-image",
      "-H:ReflectionConfigurationFiles=/home/achen2012/projects/scala3/build/native-image-reflect-config.json",
    ),
    nativeImageVersion := "21.3.0-dev",
    nativeImageInstalled := true,
    nativeImageCommand := Seq("/home/achen2012/tools/graalvm-ce-java11-21.3.0-dev/bin/native-image"),
    testFrameworks += new TestFramework("munit.Framework"),
  )
