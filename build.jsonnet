local scalaDep(org, name, version) = {
  org: org,
  name: name,
  version: version,
};

local project = std.scala.Project {
  name: "parser",
  dependencies: [],
  sources: ["modules/parser/src/main/scala"],
  scalaVersion: "3.0.2",
  libraries: [
    scalaDep("org.typelevel", "shapeless3-deriving_3", "3.0.3"),
    scalaDep("org.typelevel", "cats-parse_3", "0.3.4"),
    scalaDep("io.get-coursier", "coursier_2.13", "2.0.16"),
    scalaDep("io.get-coursier", "coursier-launcher_2.13", "2.0.16"),
    scalaDep("com.typesafe.slick", "slick_2.13", "3.3.3"),
    scalaDep("org.xerial", "sqlite-jdbc", "3.36.0.3"),
    scalaDep("org.slf4j", "slf4j-nop", "1.6.4"),
    scalaDep("ch.epfl.scala", "bsp4j", "2.0.0"),
    scalaDep("ch.epfl.scala", "bloop-launcher-core_2.13", "1.4.9-20-2c23b6ba-20211002-2109"),
    scalaDep("com.github.scopt", "scopt_3", "4.0.1"),
    scalaDep("ch.epfl.scala", "bloop-config_2.13", "1.4.9"),
  ],
  runtimeJvmHome: "/home/turtle/.cache/coursier/jvm/graalvm-java11@21.2.0/",
  runtimeJavaOpts: [
    "-agentlib:native-image-agent=config-output-dir=" + std.workspace.name + "/native-image-agent-config-output-dir"
    //"-agentpath:/home/achen2012/tools/async-profiler-2.0-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html"
  ],
};

{
  b: std.print(project.name),
  a: {
    java(args): std.print(std.runJob({cmdline: ["java", "--help"], envVars: { PATH: "/nix/store/c6svk6hihklhc3c3sy0fhfw877sj0866-openjdk-16+36/bin" }, inputFiles: []}).stdout, 0),
  },
  bloop: std.write(std.workspace.name + "/.bloop/parser.json", std.toString(project.bloopConfig)),
  java(args): std.runJob({cmdline: ["java", "--help"], envVars: { PATH: "/nix/store/c6svk6hihklhc3c3sy0fhfw877sj0866-openjdk-16+36/bin" }, inputFiles: []}).stdout,
  write(args): std.write("parser", args[0]),
  // bloopConfig(project)/// + (import "test.jsonnet")
  classpath(args): std.scala.classpath("parser", [self.bloop]),
  compile: std.scala.compile(project.bloopConfig),
}
// { a: std.scala.compile("parser", []) }
