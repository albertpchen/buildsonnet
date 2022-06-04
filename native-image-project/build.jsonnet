local deps = import 'deps.jsonnet';
local scala3Version = "3.2.0-RC1-bin-20220602-42b5941-NIGHTLY";

{
  A: std.scala.Project {
    name: 'A',
    sources: ['modules/A/src'],
    scalaVersion: scala3Version,
    libraries: [
      deps['cats-effect'],
    ]
  },

  ATest: std.scala.Project {
    name: 'A-test',
    sources: ['modules/A/test'],
    scalaVersion: scala3Version,
    dependencies: [$.A],
    libraries: [
      deps['weaver-cats'],
    ]
  },

  run: [
    $.A.compile,
    $.A.classpathString,
    $.A.mainClasses,
    $.A.run(["a.HelloWorld", "hello", "world"]),
    $.A.runFork(["a.HelloWorld", "hello", "world"]),
    std.runJob({
      local input = $.A.classpathPaths[-1],
      local output = std.runJob({
        cmdline: ["mkdir", "-p", "build/output"],
        inputFiles: [],
        outputFiles: ["build/output"],
        envVars: { PATH: std.getenv("PATH") },
      }).outputs[0],
      cmdline: ["cp", input, output],
      inputFiles: [input],
      outputFiles: [output],
      envVars: { PATH: std.getenv("PATH") },
    }),
    $.ATest.test([]),
  ],
}
