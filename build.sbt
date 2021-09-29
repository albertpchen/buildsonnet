val scala3Version = "3.0.2"

lazy val root = project.in(file("."))

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
      ("com.typesafe.slick" %% "slick" % "3.3.3").cross(CrossVersion.for3Use2_13),
      "org.slf4j" % "slf4j-nop" % "1.6.4",
      "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
    ),
    nativeImageOptions ++= List(
      //"-H:TempDirectory=" + nativeImageOutput.value.getAbsolutePath,
      "-H:ReflectionConfigurationFiles=" + (root / baseDirectory).value.getAbsolutePath + "/native-image-reflect-config.json",
      "--initialize-at-build-time",
    ),
    nativeImageVersion := "21.2.0",
    testFrameworks += new TestFramework("munit.Framework"),
  )

onLoad in Global ~= (_ andThen ("project parser" :: _))
