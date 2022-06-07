local crossVersionMap = {
  for3Use2_13: "_2.13"
};

local resolveLibraryDep(scalacConfig, dep) =
  if dep.type == "java" then
    dep
  else if "crossVersion" in dep then
    dep + {
      name+: scalacConfig.platformSuffix + crossVersionMap[dep.crossVersion]
    }
  else
    dep + {
      name+: scalacConfig.suffix
    };

local getScalacConfig(project) =
  local scalaVersion = project.scalaVersion;
  local scalaId =
    if std.startsWith(scalaVersion, '3.') then '3'
    else if std.startsWith(scalaVersion, '2.11.') then '2.11'
    else if std.startsWith(scalaVersion, '2.12.') then '2.12'
    else if std.startsWith(scalaVersion, '2.13.') then '2.13';

  if scalaId != null then
    local baseConfig =
      if std.startsWith(scalaId, '3') then {
        suffix: '_3',
        platformSuffix: '',
        compilerJars: std.scala.fetchDeps([std.scala.Dep("org.scala-lang", "scala3-compiler_3", scalaVersion)]),
        libraryDeps: [std.scala.Dep("org.scala-lang", "scala3-library_3", scalaVersion)],
        args: [],
      } else {
        suffix: '_' + scalaId,
        platformSuffix: '',
        compilerJars: std.scala.fetchDeps([std.scala.Dep("org.scala-lang", "scala-compiler", scalaVersion)]),
        libraryDeps: [std.scala.Dep("org.scala-lang", "scala-library", scalaVersion)],
        args: [],
      };

    if project.platform == 'jvm' then baseConfig
    else if project.platform == 'js' then
      local jsLibraryVersion = if std.startsWith(scalaId, '3') then '2.13' else scalaId;
      local scalaJsLibraryDep = std.scala.Dep(
        "org.scala-js",
        "scalajs-library_" + jsLibraryVersion,
        project.scalaJsVersion,
      );
      local jsId = if std.startsWith(project.scalaJsVersion, '1.') then '_sjs1';
      if jsId != null then
        baseConfig + {
          suffix: jsId + super.suffix,
          platformSuffix: jsId,
          libraryDeps+: [scalaJsLibraryDep],
          args+: if std.startsWith(scalaId, '3') then ["-scalajs"] else ["-Xplugin", /*TODO*/],
        };

{
  withSources: true,
  dependencies: [],
  libraries: [],
  platform: 'jvm',

  mode: 'debug',
  kind: 'commonjs',
  emitSourceMaps: true,

  local base = self,
  local scalacConfig = getScalacConfig(base),
  flattenedLibraries:
    scalacConfig.libraryDeps +
    [resolveLibraryDep(scalacConfig, dep) for dep in base.libraries] +
    std.flatMap(
      function(p) p.flattenedLibraries,
      base.dependencies,
    ),
  dependencyClasspath:
    std.uniq([p.bloopConfig.classesDir for p in base.dependencies] +
    std.flatMap(
      function(p) p.dependencyClasspath,
      base.dependencies
    )),
  bloopConfig: {
    assert scalacConfig != null: "scala version must start with one of [3.x.x, 2.13.x, 2.12.x, 2.11.x]",
    local bloopConfig = self,
    local flattenedLibraryArtifacts = std.scala.fetchDeps(base.flattenedLibraries),
    name: base.name,
    directory: ".",
    workspaceDir: workspace,
    sources: base.sources,
    dependencies: [p.bloopConfig for p in base.dependencies],
    classpath: base.dependencyClasspath + [artifact.path for artifact in flattenedLibraryArtifacts],
    out: ".bloop/" + bloopConfig.name,
    classesDir: bloopConfig.out + "/classes",
    resources: std.get(base, "resources", default=[]),
    scala: {
      organization: "org.scala-lang",
      name: "scala-compiler",
      version: base.scalaVersion,
      options: scalacConfig.args + std.get(base, 'scalacOptions', default=[]),
      jars: [artifact.path for artifact in scalacConfig.compilerJars],
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
      frameworks: [
        { names: [framework] } for framework in [
          "org.scalacheck.ScalaCheckFramework",
          "org.specs2.runner.Specs2Framework",
          "org.specs2.runner.SpecsFramework",
          "org.specs.runner.SpecsFramework",
          "org.scalatest.tools.Framework",
          "org.scalatest.tools.ScalaTestFramework",
          "com.novocode.junit.JUnitFramework",
          "weaver.framework.CatsEffect",
          "munit.Framework",
        ] + std.get(base, 'testFrameworks', default=[])
      ],
      options: {
        excludes: [ ],
        arguments: [ ]
      }
    },
    platform: if base.platform == 'jvm' then {
      name: "jvm",
      runtimeConfig: {
        options: std.get(base, "runtimeJavaOpts", default=[]),
        [if "runtimeJvmHome" in base then "home"]: base.runtimeJvmHome,
      },
      config: {
        [if "jvmHome" in base then "home"]: base.jvmHome,
        options: [
          "-Duser.dir=" + bloopConfig.workspaceDir
        ]
      },
      [if 'mainClass' in base then 'mainClass']: base.mainClass
    } else if base.platform == 'js' then {
      name: "js",
      config: {
        version: base.scalaJsVersion,
        mode: base.mode,
        kind: base.kind,
        emitSourceMaps: base.emitSourceMaps,
        toolchain: [],
        [if 'nodePath' in base then 'nodePath']: base.nodePath,
        output: bloopConfig.out + "/" + base.name + ".js",
      },
      mainClass: base.mainClass,
    },
    [if base.withSources then "resolution"]: {
      modules: [
        {
          name: artifact.name,
          organization: artifact.organization,
          version: artifact.version,
          artifacts: [{
            name: artifact.name,
            path: artifact.path,
          }],
        }
        for artifact in std.scala.fetchDeps(base.flattenedLibraries, withSources=true)
      ]
    },
  },
  compile:: std.scala.compile(self.bloopConfig),
  classpathPaths:: std.scala.classpath(self.bloopConfig),
  classpathString:: std.join(":", [path for path in self.classpathPaths]),
  classpath:: std.print(self.classpathString),
  mainClasses:: std.print(std.scala.mainClasses(self.bloopConfig)),
  testClasses:: std.scala.testClasses(self.bloopConfig),
  run(args)::
    if base.platform == 'jvm' then
      local len = std.length(args);
      assert len >= 1: 'missing main class argument';
      std.scala.run(
        project=self.bloopConfig,
        main=args[0],
        environmentVariables=[
          "JAVA_HOME=" + if "runtimeJvmHome" in base then base.runtimeJvmHome else std.getenv("JAVA_HOME"),
          "HOME=" + std.getenv("HOME"),
        ],
        jvmOptions=std.get(base, "runtimeJavaOpts", default=[]),
        args=if len > 1 then args[1:] else [],
      )
    else if base.platform == 'js' then
      std.scala.run(
        project=self.bloopConfig,
        main=base.mainClass,
        args=args,
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
  test(args)::
    std.scala.test(
      project=self.bloopConfig,
      args=args,
    ),
}
