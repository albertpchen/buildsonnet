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
      ("io.get-coursier" %% "coursier-launcher" % "2.0.16").cross(CrossVersion.for3Use2_13),
      ("com.typesafe.slick" %% "slick" % "3.3.3").cross(CrossVersion.for3Use2_13),
      "org.slf4j" % "slf4j-nop" % "1.6.4",
      "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
      "ch.epfl.scala" % "bsp4j" % "2.0.0",
      // ("ch.epfl.scala" %% "bloop-launcher-core" % "1.4.9-20-2c23b6ba-20211002-2109").cross(CrossVersion.for3Use2_13),
      ("ch.epfl.scala" %% "bloop-launcher-core" % "1.4.9-20-2c23b6ba-20211003-1709").cross(CrossVersion.for3Use2_13),
    ),
    nativeImageOptions ++= List(
      //"-H:TempDirectory=" + nativeImageOutput.value.getAbsolutePath,
      "--no-server",
      "--enable-http",
      "--enable-https",
      "-H:EnableURLProtocols=http,https",
      "--enable-all-security-services",
      "--no-fallback",
      "--allow-incomplete-classpath",
      "-H:+ReportExceptionStackTraces",
      "--initialize-at-build-time=scala.Symbol",
      "--initialize-at-build-time=scala.Function1",
      "--initialize-at-build-time=scala.Function2",
      "--initialize-at-build-time=scala.runtime.StructuralCallSite",
      "--initialize-at-build-time=scala.runtime.EmptyMethodCache",
      
      "-H:+PrintClassInitialization",
      "--report-unsupported-elements-at-runtime",

      "-H:ReflectionConfigurationFiles=" + (root / baseDirectory).value.getAbsolutePath + "/native-image-reflect-config.json",
      "-H:DynamicProxyConfigurationFiles=" + (root / baseDirectory).value.getAbsolutePath + "/proxy-config.json",
      "-H:JNIConfigurationFiles=" + (root / baseDirectory).value.getAbsolutePath + "/jni-reflection-config.json",
      "-H:ResourceConfigurationFiles=" + (root / baseDirectory).value.getAbsolutePath + "/native-image-resource-config.json",
      "-H:Log=registerResource:",
      // "--initialize-at-build-time",
      //"--enable-url-protocols=https",
    ),
    nativeImageVersion := "21.2.0",
    testFrameworks += new TestFramework("munit.Framework"),
  )

onLoad in Global ~= (_ andThen ("project parser" :: _))
