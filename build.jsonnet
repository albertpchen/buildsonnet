{
  bloopgun: std.scala.Project {
    name: "bloopgun",
    sources: [
      "bloop/bloopgun/src/main/scala",
      std.write("bloop/bloopgun/generated_src/BloopgunInfo.scala", |||
        package bloopgun.internal.build
        
        case object BloopgunInfo {
          val version: String = "1.3.4+186-db2dcbcb"
        }
      |||)
    ],
    scalaVersion: "2.13.6",
    libraries: [
      std.scala.Dep("me.vican.jorge", "snailgun-cli", "0.4.0"),
      std.java.Dep("org.zeroturnaround", "zt-exec", "1.11"),
      std.java.Dep("org.slf4j", "slf4j-nop", "1.7.2"),
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16"),
      std.scala.Dep("io.get-coursier", "coursier-cache", "2.0.16"),
      std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-core", "2.4.0"),
      std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-macros", "2.4.0"),
      std.java.Dep("org.bouncycastle", "bcprov-jdk15on", "1.64"),
      std.java.Dep("org.bouncycastle", "bcpkix-jdk15on", "1.64"),
    ],
  },
  bloopLauncher: std.scala.Project {
    name: "bloop-launcher",
    dependencies: [$.bloopgun],
    sources: ["bloop/launcher/src/main/scala"],
    scalaVersion: "2.13.6",
    libraries: [
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16"),
      std.scala.Dep("io.get-coursier", "coursier-cache", "2.0.16"),
      std.java.Dep("org.scala-sbt.ipcsocket", "ipcsocket", "1.4.0",),
    ],
  },
  parser213: std.scala.Project {
    name: "parser213",
    sources: ["modules/parser213/src/main/scala"],
    scalaVersion: "2.13.6",
    libraries: [
      std.scala.Dep("ch.epfl.scala", "bsp4s", "2.0.0"),
    ],
  },
  parser: std.scala.Project {
    name: "parser",
    dependencies: [$.bloopLauncher, $.parser213],
    sources: ["modules/parser/src/main/scala"],
    scalaVersion: "3.0.2",
    libraries: [
      std.scala.Dep("org.typelevel", "cats-parse", "0.3.4", crossVersion="for3Use2_13"),
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16", crossVersion="for3Use2_13"),
      std.scala.Dep("io.get-coursier", "coursier-launcher", "2.0.16", crossVersion="for3Use2_13"),
      std.scala.Dep("com.typesafe.slick", "slick", "3.3.3", crossVersion="for3Use2_13"),
      std.java.Dep("org.xerial", "sqlite-jdbc", "3.36.0.3"),
      std.scala.Dep("ch.epfl.scala", "bsp4s", "2.0.0", crossVersion="for3Use2_13"),
      std.scala.Dep("ch.epfl.scala", "bloop-config", "1.4.9", crossVersion="for3Use2_13"),
      std.scala.Dep("com.lihaoyi", "sourcecode", "0.2.7"),
    ],
    runtimeJvmHome: std.getenv("HOME") + "/.cache/coursier/jvm/graalvm-java11@21.2.0/",
    runtimeJavaOpts: [
      "-agentlib:native-image-agent=config-output-dir=" + std.workspace.name + "/native-image-agent-config-output-dir"
      //"-agentpath:/home/achen2012/tools/async-profiler-2.0-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html"
    ],
  },
}
