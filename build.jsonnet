local scala3Version = "3.1.2";
local scala213Version = "2.13.6";

local deps = {
  'cats-core': std.scala.Dep("org.typelevel", "cats-core", "2.7.0"),
  'cats-mtl': std.scala.Dep("org.typelevel", "cats-mtl", "1.2.1"),
  'cats-effect': std.scala.Dep("org.typelevel", "cats-effect", "3.3.8"),
  'cats-retry': std.scala.Dep("com.github.cb372", "cats-retry", "3.1.0"),
  'cats-parse': std.scala.Dep("org.typelevel", "cats-parse", "0.3.7"),

  'log4cats-core': std.scala.Dep("org.typelevel", "log4cats-core", "2.3.0"),

  circe(name): std.scala.Dep("io.circe", name, "0.14.1"),
  'circe-core': $.circe('circe-core'),
  'circe-generic': $.circe('circe-generic'),

  http4s(name): std.scala.Dep("org.http4s", name, "0.23.11"),
  'http4s-core': $.http4s("http4s-core"),
  'http4s-dsl': $.http4s("http4s-dsl"),
  'http4s-circe': $.http4s("http4s-circe"),
  'http4s-ember-server': $.http4s("http4s-ember-server"),
  'http4s-ember-client': $.http4s("http4s-ember-client"),
  'http4s-jwt-auth': std.scala.Dep("dev.profunktor", "http4s-jwt-auth", "1.0.0+150-26e4fd68-SNAPSHOT"),

  'squants': std.scala.Dep("org.typelevel", "squants", "1.8.3"),

  'fs2-core': std.scala.Dep("co.fs2", "fs2-core", "3.2.7"),
  'fs2-io': std.scala.Dep("co.fs2", "fs2-io", "3.2.7"),

  'refined': std.scala.Dep("eu.timepit", "refined", "0.9.28"),
  'refined-cats': std.scala.Dep("eu.timepit", "refined-cats", "0.9.28"),

  'skunk-core': std.scala.Dep("org.tpolecat", "skunk-core", "0.2.3"),
  'skunk-circe': std.scala.Dep("org.tpolecat", "skunk-circe", "0.2.3"),

  redis4cats(name): std.scala.Dep("dev.profunktor", name, "1.1.1"),
  'redis4cats-effects': $.redis4cats('redis4cats-effects'),
  'redis4cats-streams': $.redis4cats('redis4cats-streams'),
  'redis4cats-log4cats': $.redis4cats('redis4cats-log4cats'),

  weaver(name): std.scala.Dep("com.disneystreaming", "weaver-" + name, "0.7.11"),
  'weaver-cats': $.weaver('cats'),
  'weaver-scalacheck': $.weaver('scalacheck'),
  monix: std.scala.Dep("io.monix", "monix", "3.4.0"),
  scribe: std.scala.Dep("com.outr", "scribe", "3.5.5"),
  # scribeFile: std.scala.Dep("com.outr" %% "scribe-file" % "3.5.5" % Test),
  'jsoniter-core': std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-core", "2.13.8"),
  'jsoniter-macros': std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-macros", "2.13.8"),
  # "io.monix" %% "minitest" % "2.9.6" % Test,
  # "com.lihaoyi" %% "pprint" % "0.6.6" % Test
  'weaver-cats': std.scala.Dep("com.disneystreaming", "weaver-cats", "0.7.11"),
  'weaver-scalacheck': std.scala.Dep("com.disneystreaming", "weaver-scalacheck", "0.7.11"),

  parboiled: std.scala.Dep("org.parboiled", "parboiled", "2.4.0"),

  'bloop-launcher': std.scala.Dep("ch.epfl.scala", "bloop-launcher", "1.5.0", crossVersion='for3Use2_13'),
  'decline-effect': std.scala.Dep("com.monovore", "decline-effect", "2.2.0"),
};

{
  jsonrpc4cats: std.scala.Project {
    name: 'jsonrpc4cats',
    sources: ['modules/jsonrpc4cats/src'],
    scalaVersion: scala3Version,
    libraries: [
      deps['cats-effect'],
      deps['log4cats-core'],
      deps['jsoniter-core'],
      deps['jsoniter-macros'],
      deps['fs2-core'],
      deps['fs2-io'],
    ]
  },

  'jsonrpc4cats-test': std.scala.Project {
    name: 'jsonrpc4cats-test',
    sources: ['modules/jsonrpc4cats/test'],
    scalaVersion: scala3Version,
    dependencies: [$.jsonrpc4cats],
    libraries: [
      deps['weaver-cats'],
      deps['weaver-scalacheck'],
    ]
  },

  bsp4s: std.scala.Project {
    name: 'bsp4s',
    sources: ['build-server-protocol/bsp4s/src/main/scala'],
    scalaVersion: scala3Version,
    dependencies: [$.jsonrpc4cats],
  },

  logger: std.scala.Project {
    name: 'logger',
    sources: ['modules/logger/src'],
    scalaVersion: scala3Version,
    libraries: [
      deps['cats-effect'],
    ],
  },

  bsp: std.scala.Project {
    name: 'bsp',
    sources: ['modules/bsp'],
    scalaVersion: scala3Version,
    dependencies: [
      $.bsp4s,
      $.jsonrpc4cats,
      $.logger,
    ],
    libraries: [
      deps['bloop-launcher'],
    ],
  },

  ast: std.scala.Project {
    name: 'ast',
    sources: ['modules/ast/src'],
    scalaVersion: scala3Version,
    libraries: [
      deps.parboiled,
      deps['cats-parse'],
    ]
  },

  evaluator: std.scala.Project {
    name: 'evaluator',
    sources: ['modules/evaluator'],
    scalaVersion: scala3Version,
    dependencies: [
      $.logger,
      $.ast,
    ],
  },

  buildsonnet: std.scala.Project {
    name: 'buildsonnet',
    sources: ['modules/buildsonnet/src'],
    scalaVersion: scala3Version,
    dependencies: [
      $.ast,
      $.logger,
      $.bsp,
      $.evaluator,
    ],
    libraries: [
      deps['decline-effect'],
    ],
    javaRuntimeOpts: [
      "-Dcats.effect.stackTracingMode=full",
      "-Dcats.effect.traceBufferSize=2048",
    ]
  },


  ecs: std.scala.Project {
    name: "ecs",
    sources: ["modules/ecs/src"],
    scalaVersion: scala3Version,
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
      std.scala.Dep("org.typelevel", "cats-parse", "0.3.y", crossVersion="for3Use2_13"),
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
      '--in', 'docs/src/README.md',
      '--out', 'README.md',
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
