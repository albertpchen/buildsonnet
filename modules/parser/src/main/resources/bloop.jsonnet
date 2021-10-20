local suffixMap = {
  '3.': function(version) {
    suffix: '_3',
    compilerJars: std.scala.cs([std.scala.Dep("org.scala-lang", "scala3-compiler_3", version)]),
    libraryDeps: [std.scala.Dep("org.scala-lang", "scala3-library_3", version)],
  },
  '2.11.': function(version) {
    suffix: '_2.11',
    compilerJars: std.scala.cs([std.scala.Dep("org.scala-lang", "scala-compiler", version)]),
    libraryDeps: [std.scala.Dep("org.scala-lang", "scala-library", version)],
  },
  '2.12.': function(version) {
    suffix: '_2.12',
    compilerJars: std.scala.cs([std.scala.Dep("org.scala-lang", "scala-compiler", version)]),
    libraryDeps: [std.scala.Dep("org.scala-lang", "scala-library", version)],
  },
  '2.13.': function(version) {
    suffix: '_2.13',
    compilerJars: std.scala.cs([std.scala.Dep("org.scala-lang", "scala-compiler", version)]),
    libraryDeps: [std.scala.Dep("org.scala-lang", "scala-library", version)],
  },
};

local crossVersionMap = {
  for3Use2_13: "_2.13"
};

local resolveLibraryDep(scalacConfig, dep) =
  if dep.type == "java" then
    dep
  else if "crossVersion" in dep then
    dep + {
      name+: crossVersionMap[dep.crossVersion]
    }
  else
    dep + {
      name+: scalacConfig.suffix
    };

{
  local base = self,
  local scalacConfig =
    if std.startsWith(base.scalaVersion, '3.') then suffixMap['3.'](base.scalaVersion)
    else if std.startsWith(base.scalaVersion, '2.11.') then suffixMap['2.11.'](base.scalaVersion)
    else if std.startsWith(base.scalaVersion, '2.12.') then suffixMap['2.12.'](base.scalaVersion)
    else if std.startsWith(base.scalaVersion, '2.13.') then suffixMap['2.13.'](base.scalaVersion),
  local dependencies = std.get(self, 'dependencies', default=[]),
  local libraries = [resolveLibraryDep(scalacConfig, dep) for dep in std.get(self, 'libraries', default=[])],
  flattenedLibraries:
    scalacConfig.libraryDeps +
    libraries +
    std.flatMap(
      function(p) p.flattenedLibraries,
      dependencies,
    ),
  dependencyClasspath:
    std.uniq([p.bloopConfig.classesDir for p in dependencies] +
    std.flatMap(
      function(p) p.dependencyClasspath,
      dependencies
    )),
  bloopConfig: {
    assert scalacConfig != null: "scala version must start with one of" + std.objectFields(suffixMap),
    local bloopConfig = self,
    name: base.name,
    directory: ".",
    workspaceDir: std.workspace,
    sources: base.sources,
    dependencies: [p.bloopConfig for p in std.get(base, 'dependencies', default=[])],
    classpath: base.dependencyClasspath + [path.name for path in std.scala.cs(base.flattenedLibraries)],
    out: ".bloop/" + bloopConfig.name,
    classesDir: bloopConfig.out + "/classes",
    resources: std.get(base, "resources", default=[]),
    scala: {
      organization: "org.scala-lang",
      name: "scala-compiler",
      version: base.scalaVersion,
      options: std.get(base, 'scalacOptions', default=[]),
      jars: scalacConfig.compilerJars,
      analysis: bloopConfig.out + "/inc_compile_3.zip",
      setup: {
        "order": "mixed",
        "addLibraryToBootClasspath": true,
        "addCompilerToClasspath": false,
        "addExtraJarsToClasspath": false,
        "manageBootClasspath": true,
        "filterLibraryFromClasspath": true
      }
    },
    java: { options: [ ] },
    test: {
      frameworks: [{ names: ["munit.Framework"] }],
      options: {
        excludes: [ ],
        arguments: [ ]
      }
    },
    platform: {
      name: "jvm",
      runtimeConfig: {
        options: std.get(base, "runtimeJavaOpts", default=[]),
        [if "runtimeJvmHome" in base then "home"]: base.runtimeJvmHome,
      },
      config: {
        [if "jvmHome" in base then "home"]: base.jvmHome,
        options: [
          "-Duser.dir=" + bloopConfig.workspaceDir.name
        ]
      },
      [if 'mainClass' in base then 'mainClass']: base.mainClass
    },
  },
  compile:: std.scala.compile(self.bloopConfig),
  classpathString:: std.join(":", [path.name for path in std.scala.classpath(self.bloopConfig)]),
  classpath:: std.print(self.classpathString),
  mainClasses:: std.print(std.scala.mainClasses(self.bloopConfig)),
  run(args)::
    local len = std.length(args);
    assert len >= 1: 'missing main class argument';
    std.scala.run(
      project=self.bloopConfig,
      jvmOptions=std.get(base, "runtimeJavaOpts", default=[]),
      environmentVariables=[],
      main=args[0],
      args=if len > 1 then args[1:] else [],
    ),
  runFork(args)::
    local extraArgs = std.get(base, "runtimeJavaOpts", default=[]);
    local cmdline = ['java', '-cp', self.classpathString] + extraArgs + args;
    local jvmHome =
      if "runtimeJvmHome" in base then
        base.runtimeJvmHome
      else
        std.getenv("JAVA_HOME");
    std.runJob({
      cmdline: cmdline,
      envVars: {
        PATH: jvmHome + "/bin",
        HOME: std.getenv("HOME"),
        JAVA_HOME: jvmHome,
      },
      inputFiles: []
    }),
}
