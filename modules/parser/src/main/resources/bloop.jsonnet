local scalaDep(org, name, version) = {
  org: org,
  name: name,
  version: version,
};

{
  local base = self,
  local suffixMap = {
    '3.': function(version) { suffix: '_3', compilerJars: std.scala.cs([scalaDep("org.scala-lang", "scala3-compiler_3", version)]) },
    '2.11.': function(version) { suffix: '_2.11', compilerJars: std.scala.cs([scalaDep("org.scala-lang", "scala-compiler", version)]) },
    '2.12.': function(version) { suffix: '_2.12', compilerJars: std.scala.cs([scalaDep("org.scala-lang", "scala-compiler", version)]) },
    '2.13.': function(version) { suffix: '_2.13', compilerJars: std.scala.cs([scalaDep("org.scala-lang", "scala-compiler", version)]) },
  },

  local dependencies = std.get(self, 'dependencies', default=[]),
  flattenedLibraries:
    self.libraries +
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
    local scalacConfig =
      if std.startsWith(base.scalaVersion, '3.') then suffixMap['3.'](base.scalaVersion)
      else if std.startsWith(base.scalaVersion, '2.11.') then suffixMap['2.11.'](base.scalaVersion)
      else if std.startsWith(base.scalaVersion, '2.12.') then suffixMap['2.12.'](base.scalaVersion)
      else if std.startsWith(base.scalaVersion, '2.13.') then suffixMap['2.13.'](base.scalaVersion),
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
  classpath:: std.print([path.name for path in std.scala.classpath(self.bloopConfig)]),
}
