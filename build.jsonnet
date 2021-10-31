local scala3Version = "3.0.2";
local scala213Version = "2.13.6";
{
  ecs: std.scala.Project {
    name: "ecs",
    sources: ["modules/ecs/src"],
    scalaVersion: scala3Version,
  },

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
    scalaVersion: scala213Version,
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
    scalaVersion: scala213Version,
    libraries: [
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16"),
      std.scala.Dep("io.get-coursier", "coursier-cache", "2.0.16"),
      std.java.Dep("org.scala-sbt.ipcsocket", "ipcsocket", "1.4.0",),
    ],
  },

  parser213: std.scala.Project {
    name: "parser213",
    sources: ["modules/parser213/src/main/scala"],
    scalaVersion: scala213Version,
    libraries: [
      std.scala.Dep("ch.epfl.scala", "bsp4s", "2.0.0"),
    ],
  },

  js: std.scala.Project {
    name: "js",
    platform: "js",
    scalaVersion: "3.0.2",
    scalaJsVersion: "1.7.1",
    sources: ["modules/js/src"],
    withSources: true,
    mode: 'debug',
    mainClass: 'tutorial.webapp.hello',
    nodePath: '/nix/store/s1cc19ypzs09mnhzlyrvl6ml44nkx7yy-nodejs-14.18.0/bin/node',
    libraries: [
      // std.scala.Dep("org.scala-js", "scalajs-dom", "2.0.0",),
      // available for 2.12, 2.13, 3.0
      std.scala.Dep("co.fs2", "fs2-core", "3.1.6"),

      // optional I/O library
      std.scala.Dep("co.fs2", "fs2-io", "3.1.6"),

      // // optional reactive streams interop
      // std.scala.Dep("co.fs2", "fs2-reactive-streams", "3.2.0"),

      // // optional scodec interop
      // std.scala.Dep("co.fs2", "fs2-scodec", "3.1.6"),
    ],
  },
  parser: std.scala.Project {
    name: "parser",
    withSources: true,
    dependencies: [$.bloopLauncher, $.parser213],
    sources: ["modules/parser/src/main/scala"],
    scalaVersion: scala3Version,
    scalacOptions: ["-Xmax-inlines", "100"],
    libraries: [
      std.scala.Dep("org.typelevel", "cats-parse", "0.3.4", crossVersion="for3Use2_13"),
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16", crossVersion="for3Use2_13"),
      std.scala.Dep("io.get-coursier", "coursier-jvm", "2.0.16", crossVersion="for3Use2_13"),
      std.scala.Dep("io.get-coursier", "coursier-launcher", "2.0.16", crossVersion="for3Use2_13"),
      std.scala.Dep("com.typesafe.slick", "slick", "3.3.3", crossVersion="for3Use2_13"),
      std.java.Dep("org.xerial", "sqlite-jdbc", "3.36.0.3"),
      std.scala.Dep("ch.epfl.scala", "bsp4s", "2.0.0", crossVersion="for3Use2_13"),
      std.scala.Dep("ch.epfl.scala", "bloop-config", "1.4.9", crossVersion="for3Use2_13"),
      std.scala.Dep("com.lihaoyi", "sourcecode", "0.2.7"),
    ],
    runtimeJvmHome: std.getenv("HOME") + "/.cache/coursier/jvm/graalvm-java11@21.2.0/",
    runtimeJavaOpts: [
      // "-agentpath:/path/to/libasyncProfiler.so=start,event=cpu,file=profile.html",
      // "-agentlib:native-image-agent=config-output-dir=" + std.workspace.name + "/native-image-agent-config-output-dir"
    ],
  },

  glob(args):
    local syntax = if std.length(args) >= 3 then args[2] else "glob";
    local paths = std.paths(args[0], args[1], syntax);
    std.print([path.name for path in paths]),

  help(args):
    local jvmHome = std.scala.jvm("graalvm-java11:21.2.0").name;
    std.runJob({
      cmdline: [jvmHome + "/bin/native-image", "--help"],
      inputFiles: [],
    }),

  cpp(args):
    local paths = std.paths("modules/cpp/src", "**.cpp");
    std.runJob({
      cmdline: [std.getenv("CXX"), "-std=c++17", "-o", "HelloWorld"] + args + [path.name for path in paths],
      inputFiles: paths,
      outputFiles: ["HelloWorld"],
      envVars: {
        PATH: std.getenv("PATH"),
        LIBRARY_PATH: std.getenv("LIBRARY_PATH"),
      }
    }),

  nativeImage(args):
    local jvmHome = std.scala.jvm("graalvm-java11:21.2.0").name;
    local workspaceDir = std.workspace.name;
    local nativeImageConfigDir = workspaceDir + "/native-image-agent-config-output-dir";
    std.runJob({
      cmdline: [
        jvmHome + "/bin/native-image",
        "-cp", $.parser.classpathString
      ] + [
        "--no-server",
        "--enable-http",
        "--enable-https",
        "-H:EnableURLProtocols=http,https",
        "--enable-all-security-services",
        "--no-fallback",
        "--allow-incomplete-classpath",
        "-H:+ReportExceptionStackTraces",

        "-H:+PrintClassInitialization",
        "--report-unsupported-elements-at-runtime",

        "-H:ReflectionConfigurationFiles=" + workspaceDir + "/native-image-reflect-config.json",
        "-H:ResourceConfigurationFiles=" + workspaceDir + "/native-image-resource-config.json",

        "-H:ReflectionConfigurationFiles=" + nativeImageConfigDir + "/reflect-config.json",
        "-H:ResourceConfigurationFiles=" + nativeImageConfigDir + "/resource-config.json",
        "-H:JNIConfigurationFiles=" + nativeImageConfigDir + "/jni-config.json",

        "--initialize-at-build-time=scala.Symbol",
        "--initialize-at-build-time=scala.Function1",
        "--initialize-at-build-time=scala.Function2",
        "--initialize-at-build-time=scala.runtime.StructuralCallSite",
        "--initialize-at-build-time=scala.runtime.EmptyMethodCache",

        "--initialize-at-run-time=scribe.Logger$",
        "--initialize-at-run-time=scribe.LoggerId$",
      ] + args,
      inputFiles: std.scala.classpath($.parser.bloopConfig),
      envVars: {
        PATH: std.getenv("PATH"),
        LIBRARY_PATH: std.getenv("LIBRARY_PATH"),
      }
    }),
  mdoc(args):
    local classpath = std.join(":", [path.name for path in std.scala.cs([std.scala.Dep("org.scalameta", "mdoc_3", "2.2.24")])]);
    local cmdline = [
      'java',
      '-cp', classpath,
      'mdoc.Main',
      '--classpath', $.parser.classpathString,
      '--in', 'docs/src/usage.md',
      '--out', 'docs',
    ] + args;
    local jvmHome =
      if "runtimeJvmHome" in $.parser then
        $.parser.runtimeJvmHome
      else
        std.getenv("JAVA_HOME");
    std.runJob({
      cmdline: cmdline,
      envVars: {
        PATH: jvmHome + "/bin",
        HOME: std.getenv("HOME"),
        JAVA_HOME: jvmHome,
      },
      inputFiles: $.parser.classpathPaths + std.paths('docs/src', '**'),
    }),
}
