local ScalaProject = {
  ivyDep:: function (org, name, version) {
    org: org,
    name: name,
    version: version,
  },
  scalaVersion: "3.0.2",

  bloopVersion: "1.4.0", // OPTIONAL
  workspaceDir: "/home/achen2012/projects/scala3/build", // OPTIONAL
  local project = self,
  bloop: {
    out: ".bloop/" + project.name, // OPTIONAL
    analysis: self.out  + "/inc_compile_3.zip", //OPTIONAL
    classesDir: self.out + "/scala-3/classes",
  }
};

local bloopConfig(project) = {
  version: project.bloopVersion,
  project: {
    name: project.name,
    directory: ".",
    workspaceDir: project.workspaceDir,
    sources: project.sources,
    dependencies: std.get(project, "dependencies", default=[]),
    classpath: std.scala.cs(project.libraries),
    out: project.bloop.out,
    classesDir: project.bloop.classesDir,
    resources: std.get(project, "resources", default=[]),
    scala: {
      organization: "org.scala-lang",
      name: "scala-compiler",
      version: "3.0.2",
      options: [ ],
      jars: std.scala.cs([project.ivyDep("org.scala-lang", "scala3-compiler_3", "3.0.2")]),
      analysis: project.bloop.out + "/inc_compile_3.zip",
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
        options: std.get(project, "runtimeJavaOpts", default=[]),
      },
      config: {
        options: [
          "-Duser.dir=" + project.workspaceDir
        ]
      },
      mainClass: [ ]
    },
  }
};

local project = ScalaProject {
  name: "my-parser",
  directory: "modules/parser",
  dependencies: [],
  sources: [self.directory + "/src/main/scala"],
  libraries: [
    self.ivyDep("org.typelevel", "shapeless3-deriving_3", "3.0.3"),
    self.ivyDep("org.typelevel", "cats-parse_3", "0.3.4",),
    self.ivyDep("io.get-coursier", "coursier_2.13", "2.0.16"),
  ],
  runtimeJavaOpts: [
    "-agentpath:/home/achen2012/tools/async-profiler-2.0-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html"
  ]
};
bloopConfig(project)
