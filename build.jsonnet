local ScalaProject = {
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
};

local bloopConfig(project) = {
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
};

local project = ScalaProject {
  name: "parser",
  dependencies: [],
  sources: ["modules/parser/src/main/scala"],
  libraries: [
    $.scalaDep("org.typelevel", "shapeless3-deriving_3", "3.0.3"),
    $.scalaDep("org.typelevel", "cats-parse_3", "0.3.4"),
    $.scalaDep("io.get-coursier", "coursier_2.13", "2.0.16"),
    $.scalaDep("io.get-coursier", "coursier-launcher_2.13", "2.0.16"),
    $.scalaDep("com.typesafe.slick", "slick_2.13", "3.3.3"),
    $.scalaDep("org.xerial", "sqlite-jdbc", "3.36.0.3"),
    $.scalaDep("org.slf4j", "slf4j-nop", "1.6.4"),
    $.scalaDep("ch.epfl.scala", "bsp4j", "2.0.0"),
    $.scalaDep("ch.epfl.scala", "bloop-launcher-core_2.13", "1.4.9-20-2c23b6ba-20211002-2109"),
    $.scalaDep("com.github.scopt", "scopt_3", "4.0.1"),
    $.scalaDep("ch.epfl.scala", "bloop-config_2.13", "1.4.9"),
  ],
  runtimeJvmHome: "/home/turtle/.cache/coursier/jvm/graalvm-java11@21.2.0/",
  runtimeJavaOpts: [
    "-agentlib:native-image-agent=config-output-dir=" + std.workspace() + "/native-image-agent-config-output-dir"
    //"-agentpath:/home/achen2012/tools/async-profiler-2.0-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html"
  ],
};

{
  a: {
    java(args): std.print(std.runJob({cmdline: ["java", "--help"], envVars: { PATH: "/nix/store/c6svk6hihklhc3c3sy0fhfw877sj0866-openjdk-16+36/bin" }, inputFiles: []}).stdout, 0),
  },
  java(args): std.runJob({cmdline: ["java", "--help"], envVars: { PATH: "/nix/store/c6svk6hihklhc3c3sy0fhfw877sj0866-openjdk-16+36/bin" }, inputFiles: []}).stdout,
  write(args): std.write("parser", args[0]),
  // bloopConfig(project)/// + (import "test.jsonnet")
  classpath(args): std.scala.classpath("parser", [std.write(std.workspace() + "/.bloop/asdf.json", std.toString(bloopConfig(project)))]),
}
