{
  ScalaProject: {
    scalaDep:: function (org, name, version) {
      org: org,
      name: name,
      version: version,
    },
    scalaVersion: "3.0.2",

    bloopVersion: "1.4.0", // OPTIONAL
    workspaceDir: std.workspace(),
    local project = self,
    bloop: {
      out: ".bloop/" + project.name, // OPTIONAL
      analysis: self.out  + "/inc_compile_3.zip", //OPTIONAL
      classesDir: self.out + "/scala-3/classes",
    }
  },

  bloopConfig(project): {
    version: project.bloopVersion,
    project: {
      name: project.name,
      directory: ".",
      workspaceDir: project.workspaceDir,
      sources: project.sources,
      dependencies: std.get(project, "dependencies", default=[]),
      classpath: [path.name for path in std.scala.cs(project.libraries)],
      out: project.bloop.out,
      classesDir: project.bloop.classesDir,
      resources: std.get(project, "resources", default=[]),
      scala: {
        organization: "org.scala-lang",
        name: "scala-compiler",
        version: "3.0.2",
        options: [ ],
        jars:
          local paths = std.scala.cs([project.scalaDep("org.scala-lang", "scala3-compiler_3", "3.0.2")]);
          [path.name for path in paths],
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
          [if "runtimeJvmHome" in project then "home"]: project.runtimeJvmHome,
        },
        config: {
          [if "jvmHome" in project then "home"]: project.jvmHome,
          options: [
            "-Duser.dir=" + project.workspaceDir
          ]
        },
        mainClass: [ ]
      },
    }
  },
}
