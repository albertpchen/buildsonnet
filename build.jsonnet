local deps = import 'dependencies.jsonnet';
local ScalaProject = import 'modules/buildsonnet/resources/std.scala.Project.jsonnet';

local scala3Version = "3.2.0-RC1-bin-20220602-42b5941-NIGHTLY";
local scala213Version = "2.13.6";

local BaseProject(name) = ScalaProject {
  name: name,
  sources: ['modules/' + name + '/src'],
  scalaVersion: scala3Version,
  local outer = self,
  test: ScalaProject {
    name: name + '-test',
    dependencies: [outer],
    sources: ['modules/' + name + '/test'],
    scalaVersion: scala3Version,
  }
};

{
  jsonrpc4cats: BaseProject('jsonrpc4cats') {
    libraries: [
      deps['cats-effect'],
      deps['log4cats-core'],
      deps['jsoniter-core'],
      deps['jsoniter-macros'],
      deps['fs2-core'],
      deps['fs2-io'],
    ]
  },

  jsonrpc4catsTest: $.jsonrpc4cats.test {
    libraries: [
      deps['weaver-cats'],
      deps['weaver-scalacheck'],
    ]
  },

  bsp4s: BaseProject('bsp4s') {
    sources: ['build-server-protocol/bsp4s/src/main/scala'],
    dependencies: [$.jsonrpc4cats],
  },

  logger: BaseProject('logger') {
    libraries: [
      deps['cats-effect'],
    ],
  },

  bloopLauncher: BaseProject('bloop-launcher') {
    sources: ["bloop/launcher-core/src/main/scala"],
    scalaVersion: scala213Version,
    libraries: [
      deps['coursier'],
      deps['coursier-cache'],
      deps['ipcsocket'],
      deps['bloopgun'],
    ],
  },

  bsp: BaseProject('bsp') {
    dependencies: [
      $.bsp4s,
      $.jsonrpc4cats,
      $.logger,
      $.bloopLauncher
    ],
  },

  job: BaseProject('job') {
    libraries: [
      deps['doobie-core'],
      deps['doobie-hikari'],
      deps['sqlite-jdbc'],
      deps['log4cats-core'],
    ],
    dependencies: [
      $.logger,
      $.evaluator,
    ],
  },

  ast: BaseProject('ast') {
    libraries: [
      deps['cats-parse'],
    ]
  },

  evaluator: BaseProject('evaluator') {
    dependencies: [
      $.logger,
      $.ast,
    ],
  },

  buildsonnet: BaseProject('buildsonnet') {
    dependencies: [
      $.ast,
      $.logger,
      $.bsp,
      $.evaluator,
      $.job,
    ],
    libraries: [
      deps['decline-effect'],
      deps['log4cats-slf4j'],
      deps['logback'],
      deps['coursier-jvm'],
      deps['bloop-config'],
    ],
    javaRuntimeOpts: [
      "-Dcats.effect.stackTracingMode=full",
      "-Dcats.effect.traceBufferSize=2048",
    ]
  },

  nativeImage(args):
    local jvmHome = std.java.fetchJvm("graalvm-java11:22.1.0");
    local configDir = workspace + '/' + 'build/native-image-config';
    local configFiles = std.runJob({
      cmdline: [
        jvmHome + "/bin/java",
        //"-agentpath:" + jvmHome + "/lib/libnative-image-agent.dylib" + "=config-output-dir=" + configDir,
        "-agentlib:native-image-agent=config-output-dir=" + configDir,
        "-cp", $.buildsonnet.classpathString,
        "buildsonnet.Buildsonnet", "run", "-C", "native-image-project", "--", "run",
      ],
      inputFiles: [
        "native-image-project/build.jsonnet",
        "native-image-project/deps.jsonnet",
      ] + std.find("native-image-project/modules", "**.scala"),
      outputFiles: [
        configDir + "/reflect-config.json",
        configDir + "/resource-config.json",
        configDir + "/jni-config.json",
        configDir + "/proxy-config.json",
      ],
      envVars: {
        JAVA_HOME: jvmHome,
        PATH: std.getenv("PATH"),
        HOME: std.getenv("HOME"),
        LD_LIBRARY_PATH: jvmHome + "/lib",
      },
    }).outputs;
    local nativeImage = std.runJob({
      cmdline: [
        "nix-shell", "--command",
        std.join(" ", [
          jvmHome + "/bin/native-image",
          "-cp", $.buildsonnet.classpathString,
          "--no-server",
          "--enable-http",
          "--enable-https",
          "-H:EnableURLProtocols=http,https",
          "--enable-all-security-services",
          "--no-fallback",
          "--allow-incomplete-classpath",
          "-H:+ReportExceptionStackTraces",
          "-H:+PrintClassInitialization",
          "-H:-CheckToolchain",
          "--report-unsupported-elements-at-runtime",

          "-H:ReflectionConfigurationFiles=" + configDir + "/reflect-config.json",
          "-H:ResourceConfigurationFiles=" + configDir + "/resource-config.json",
          "-H:JNIConfigurationFiles=" + configDir + "/jni-config.json",
          "-H:DynamicProxyConfigurationFiles=" + configDir + "/proxy-config.json",

          "buildsonnet.Buildsonnet", "build/bin/buildsonnet",
        ] + args),
      ],
      inputFiles: $.buildsonnet.classpathPaths + configFiles,
      envVars: {
        [var]: std.getenv(var) for var in ["PATH"]
      },
      outputFiles: ["build/bin/buildsonnet"],
      fail: false,
    });
    nativeImage.exitCode,

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
