val Dependencies = new {
  val catsCore = "org.typelevel" %% "cats-core" % "2.7.0"
  val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.8"
  val catsParse = "org.typelevel" %% "cats-parse" % "0.3.7"

  val log4catsCore = "org.typelevel" %% "log4cats-core" % "2.3.1"
  val log4catsSlf4j = "org.typelevel" %% "log4cats-slf4j" % "2.3.1"

  val fs2Core = "co.fs2" %% "fs2-core" % "3.2.7"
  val fs2Io = "co.fs2" %% "fs2-io" % "3.2.7"

  def weaver(name: String) = "com.disneystreaming" %% s"weaver-$name" % "0.7.11" % Test
  val weaverCats = weaver("cats")
  val weaverScalacheck = weaver("scalacheck")

  def jsoniter(name: String) = "com.github.plokhotnyuk.jsoniter-scala" %% s"jsoniter-scala-$name" % "2.13.29"
  val jsoniterCore = jsoniter("core")
  val jsoniterMacros = jsoniter("macros")

  val bloopLauncher = ("ch.epfl.scala" %% "bloop-launcher" % "1.5.0").cross(CrossVersion.for3Use2_13)
  val declineEffect = "com.monovore" %% "decline-effect" % "2.2.0"

  def doobie(name: String) = "org.tpolecat" %% s"doobie-$name" % "1.0.0-RC2"
  val doobieCore = doobie("core")
  val doobieHikari = doobie("hikari")

  val sqliteJdbc = "org.xerial" % "sqlite-jdbc" % "3.36.0.3"
  val logback = "ch.qos.logback" % "logback-classic" % "1.2.11"

  val coursier = ("io.get-coursier" %% "coursier" % "2.0.16").cross(CrossVersion.for3Use2_13)
  val coursierCache = ("io.get-coursier" %% "coursier-cache" % "2.0.16").cross(CrossVersion.for3Use2_13)
  val coursierJvm = ("io.get-coursier" %% "coursier-jvm" % "2.0.16").cross(CrossVersion.for3Use2_13)
  val coursierLauncher = "io.get-coursier" %% "coursier-launcher" % "2.0.16"
  val ipcsocket = "org.scala-sbt.ipcsocket" % "ipcsocket" % "1.4.0"

  val bloopConfig = ("ch.epfl.scala" %% "bloop-config" % "1.5.0").cross(CrossVersion.for3Use2_13)
  val bloopgun = ("ch.epfl.scala" %% "bloopgun" % "1.5.0").cross(CrossVersion.for3Use2_13)
}

val scala3Version = "3.2.0-RC1-bin-20220602-42b5941-NIGHTLY"
val scala213Version = "2.13.6"

lazy val root = project.in(file("."))

lazy val jsonrpc4cats = project
  .in(file("modules/jsonrpc4cats"))
  .settings(
    scalaVersion := scala3Version,
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    libraryDependencies ++= List(
      Dependencies.catsEffect,
      Dependencies.log4catsCore,
      Dependencies.jsoniterCore,
      Dependencies.jsoniterMacros,
      Dependencies.fs2Core,
      Dependencies.fs2Io,
      Dependencies.weaverCats,
      Dependencies.weaverScalacheck,
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  )

lazy val bsp4s = project
  .in(file("build-server-protocol/bsp4s"))
  .dependsOn(jsonrpc4cats)
  .settings(
    scalaVersion := scala3Version,
  )

lazy val logger = project
  .in(file("modules/logger"))
  .settings(
    scalaVersion := scala3Version,
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    libraryDependencies ++= List(
      Dependencies.catsEffect,
    ),
  )

lazy val bloopLauncher: Project = project
  .in(file("bloop/launcher-core"))
  .disablePlugins(ScriptedPlugin)
  .settings(
    name := "bloop-launcher-core",
    scalaVersion := scala213Version,
    crossVersion := CrossVersion.constant(scala3Version),
    libraryDependencies ++= List(
      Dependencies.ipcsocket,
      Dependencies.bloopgun,
    )
  )

lazy val bsp = project
  .in(file("modules/bsp"))
  .dependsOn(bsp4s, jsonrpc4cats, logger, bloopLauncher)
  .settings(
    scalaVersion := scala3Version,
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
  )

lazy val ast = project
  .in(file("modules/ast"))
  .settings(
    scalaVersion := scala3Version,
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    libraryDependencies ++= List(
      Dependencies.catsCore,
      Dependencies.catsParse,
    ),
  )

lazy val evaluator = project
  .in(file("modules/evaluator"))
  .dependsOn(ast, logger)
  .settings(
    scalaVersion := scala3Version,
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
  )

lazy val job = project
  .in(file("modules/job"))
  .dependsOn(logger, evaluator)
  .settings(
    scalaVersion := scala3Version,
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    libraryDependencies ++= List(
      Dependencies.doobieCore,
      Dependencies.doobieHikari,
      Dependencies.sqliteJdbc,
      Dependencies.log4catsCore,
    ),
  )

lazy val buildsonnet = project
  .in(file("modules/buildsonnet"))
  .enablePlugins(NativeImagePlugin)
  .dependsOn(logger, evaluator, ast, bsp, job)
  .settings(
    scalaVersion := scala3Version,
    conflictWarning := {
      if (scalaBinaryVersion.value == "3") {
        // TODO
        ConflictWarning("warn", Level.Warn, false)
      } else {
        conflictWarning.value
      }
    },
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    libraryDependencies ++= List(
      Dependencies.declineEffect,
      Dependencies.log4catsSlf4j,
      Dependencies.logback,
      Dependencies.coursierJvm,
      Dependencies.bloopConfig,
    ),
    fork := true,
    run / javaOptions ++= List(
      "-Dcats.effect.stackTracingMode=full",
      "-Dcats.effect.traceBufferSize=2048",
    ),
  )

onLoad in Global ~= (_ andThen ("project buildsonnet" :: _))
