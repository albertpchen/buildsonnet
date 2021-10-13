val scala3Version = "3.0.2"
val scala213Version = "2.13.6"

val Dependencies = new {
  val coursierVersion = "2.0.16"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierCache = "io.get-coursier" %% "coursier-cache" % coursierVersion
  val coursierLauncher = "io.get-coursier" %% "coursier-launcher" % coursierVersion

  val snailgunVersion = "0.4.0"
  val snailgun = ("me.vican.jorge" %% "snailgun-cli" % snailgunVersion)

  val jsoniterVersion = "2.4.0"
  val jsoniterCore =
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % jsoniterVersion
  val jsoniterMacros =
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion

  val slf4jNop = "org.slf4j" % "slf4j-nop" % "1.7.2"

  val ztExecVersion = "1.11"
  val ztExec = "org.zeroturnaround" % "zt-exec" % ztExecVersion
}

lazy val root = project.in(file("."))

lazy val bloopgun: Project = project
  .in(file("bloop/bloopgun"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "bloopgun-core",
    scalaVersion := scala213Version,
    buildInfoPackage := "bloopgun.internal.build",
    buildInfoKeys := List(Keys.version),
    buildInfoObject := "BloopgunInfo",
    libraryDependencies ++= List(
      //Dependencies.configDirectories,
      Dependencies.snailgun,
      // Use zt-exec instead of nuprocess because it doesn't require JNA (good for graalvm)
      Dependencies.ztExec,
      Dependencies.slf4jNop,
      Dependencies.coursier,
      Dependencies.coursierCache,
      Dependencies.jsoniterCore,
      Dependencies.jsoniterMacros % Provided,
      // Necessary to compile to native (see https://github.com/coursier/coursier/blob/0bf1c4f364ceff76892751a51361a41dfc478b8d/build.sbt#L376)
      "org.bouncycastle" % "bcprov-jdk15on" % "1.64",
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.64"
    ),
  )

lazy val bloopLauncher: Project = project
  .in(file("bloop/launcher"))
  .disablePlugins(ScriptedPlugin)
  .dependsOn(bloopgun)
  .settings(
    name := "bloop-launcher-core",
    scalaVersion := scala213Version,
    crossVersion := CrossVersion.constant(scala3Version),
    libraryDependencies ++= List(
      "org.scala-sbt.ipcsocket" % "ipcsocket" % "1.4.0",
      Dependencies.coursier,
      Dependencies.coursierCache
    )
  )

lazy val parser213 = project
  .in(file("modules/parser213"))
  .settings(
    run / fork := true,
    version := "0.1.0",
    scalaVersion := scala213Version,
    javaOptions ++= Seq(
      /// "-agentlib:native-image-agent=config-output-dir=" + (root / baseDirectory).value.getAbsolutePath + "/native-image-agent-config-output-dir"
    ),
    libraryDependencies ++= Seq(
      ("ch.epfl.scala" %% "bsp4s" % "2.0.0").cross(CrossVersion.for3Use2_13),
    ),
  )

// project for JVM build (default)
lazy val parser = project
  .in(file("modules/parser"))
  .enablePlugins(NativeImagePlugin)
  .dependsOn(bloopLauncher, parser213)
  .settings(
    run / fork := true,
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions ++= List("-Xmax-inlines", "1000000000"),
    javaOptions ++= Seq(
      /// "-agentlib:native-image-agent=config-output-dir=" + (root / baseDirectory).value.getAbsolutePath + "/native-image-agent-config-output-dir"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "shapeless3-deriving" % "3.0.3",
      ("org.typelevel" %% "cats-parse" % "0.3.4").cross(CrossVersion.for3Use2_13),
      "org.scalameta" %%% "munit" % "0.7.26" % Test,
      Dependencies.coursier.cross(CrossVersion.for3Use2_13),
      Dependencies.coursierLauncher.cross(CrossVersion.for3Use2_13),
      ("com.typesafe.slick" %% "slick" % "3.3.3").cross(CrossVersion.for3Use2_13),
      "org.slf4j" % "slf4j-nop" % "1.6.4",
      "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
      "ch.epfl.scala" % "bsp4j" % "2.0.0",
      ("ch.epfl.scala" %% "bsp4s" % "2.0.0").cross(CrossVersion.for3Use2_13),
      // ("ch.epfl.scala" %% "bloop-launcher-core" % "1.4.9-20-2c23b6ba-20211002-2109").cross(CrossVersion.for3Use2_13),
      //("ch.epfl.scala" %% "bloop-launcher-core" % "1.4.9-20-2c23b6ba-20211003-1709").cross(CrossVersion.for3Use2_13),
      ("ch.epfl.scala" %% "bloop-config" % "1.4.9").cross(CrossVersion.for3Use2_13),
      ("com.lihaoyi" %% "sourcecode" % "0.2.7"),
    ),
    nativeImageOptions ++= {
      val workspaceDir = (root / baseDirectory).value.getAbsolutePath
      val nativeImageConfigDir = workspaceDir + "/native-image-agent-config-output-dir"
      List(
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

        "-H:ReflectionConfigurationFiles=" + nativeImageConfigDir + "/reflect-config.json",
        "-H:ReflectionConfigurationFiles=" + workspaceDir + "/native-image-reflect-config.json",
        "-H:DynamicProxyConfigurationFiles=" + workspaceDir + "/proxy-config.json",
        "-H:JNIConfigurationFiles=" + nativeImageConfigDir + "/jni-config.json",
        "-H:ResourceConfigurationFiles=" + nativeImageConfigDir + "/resource-config.json",
        "-H:ResourceConfigurationFiles=" + workspaceDir + "/native-image-resource-config.json",
        "--initialize-at-build-time=ch.epfl.scala.bsp4j",
        // "-H:Log=registerResource:",
        // "--initialize-at-build-time=ch.epfl.scala.bsp4j",
        // "--initialize-at-build-time",
        // "--enable-url-protocols=https",
      )
    },
    nativeImageVersion := "21.2.0",
    testFrameworks += new TestFramework("munit.Framework"),
  )

onLoad in Global ~= (_ andThen ("project parser" :: _))
