# Overview
Buildsonnet is a work-in-progress, general-purpose build tool based on the
Jsonnet language with built-in support for building scala code through
[bloop](https://scalacenter.github.io/bloop/). Scala project configuration is
similar to [Sbt](https://www.scala-sbt.org) but done using jsonnet's object mix-in features rather than a
scala DSL. The Jsonnet standard library is extended with functions for running
command-line jobs and compiling and running scala code.

Buildsonnet can be compiled with graal native-image for fast startup times.
Scala compilations are dispatched to a bloop instance using the build server
protocol and the included bloop-launcher.

Jobs are cached using file timestamps with results stored in an sqlite
database.

# Motivation
Sbt's extensiblity comes from its lazily evaluated settings keys that are
defined using a scala DSL. It is also where a lot of my frustrations with sbt
come from. The reliance on a scala DSL makes start-up times slow, and project
configurations unnecessarily verbose and complex.

[Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html) relies on
scala inheritance for project configuration. While this approach may result in
more understandable code, it is also very verbose and requires a scala
interpreter.

The Jsonnet language provides a lot of the same functionality of sbt project
keys with a more concise syntax. Not having to run a full scala interpreter
makes start-up times fast.

# Standard Library Extensions
Buildsonnet extends the Jsonnet standard libary with helpers for running scala
build commands and running jobs. Note that these are side-effecting functions
that may modify the build workspace. This means that Buildsonnet loses the
hermeticity of Jsonnet. Currently jobs are run without any sandboxing and are
cached using timestamps. A potential future enhancement would be to run jobs
using Bazel's [sandboxfs](https://github.com/bazelbuild/sandboxfs) and use file
hashes instead of timestamps.

## `std.scala.Dep(org, name, version, crossVersion=null)`
Helper for creating a scala dependency object e.g.
```json
{
  org: '',
  name: '',
  version: '',
  crossVersion: 'for3Use2_13'
  type: 'scala'
}
```

## `std.scala.Project`
An object mix-in that can be used to create a scala project. It provides,
`bloopConfig`, `compile`, `classpath`, and `run` fields.

The parent object is required to define `name`, `scalaVersion`, and `sources`
and optionally `dependencies` and `libraries`. e.g.
```jsonnet
{
  bloopgun: std.scala.Project {
    name: "bloopgun",
    sources: [
      "bloop/bloopgun/src/main/scala",
      std.write("bloop/bloopgun/generated_src/BloopgunInfo.scala", |||
        package bloopgun.internal.build

        /** This object was generated by buildsonnet. */
        case object BloopgunInfo {
          val version: String = "0.1.0-SNAPSHOT"
          override val toString: String = "version: $version"
        }
      |||)
    ],
    scalaVersion: scala213Version,
    libraries: [
      std.scala.Dep("me.vican.jorge", "snailgun-cli", "0.4.0"),
      std.java.Dep("org.zeroturnaround", "zt-exec", "1.11"),
      std.java.Dep("org.slf4j", "slf4j-nop", "1.7.2"),
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16"),
      std.scala.Dep("io.get-coursier", "coursier-cache", "2.0.16"),
      std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-core", "2.4.0"),
      std.scala.Dep("com.github.plokhotnyuk.jsoniter-scala", "jsoniter-scala-macros", "2.4.0"),
      std.java.Dep("org.bouncycastle", "bcprov-jdk15on", "1.64"),
      std.java.Dep("org.bouncycastle", "bcpkix-jdk15on", "1.64"),
    ],
  },

  bloopLauncher: std.scala.Project {
    name: "bloop-launcher",
    dependencies: [$.bloopgun],
    sources: ["bloop/launcher/src/main/scala"],
    scalaVersion: scala213Version,
    libraries: [
      std.scala.Dep("io.get-coursier", "coursier", "2.0.16"),
      std.scala.Dep("io.get-coursier", "coursier-cache", "2.0.16"),
      std.java.Dep("org.scala-sbt.ipcsocket", "ipcsocket", "1.4.0",),
    ],
  },

  compileBloopLauncher: $.bloopLauncher.compile // compile using bloop bsp
  runBloopLauncher(args): $.bloopLauncher.run(args) // run using bloop bsp
  runForkBloopLauncher(args): $.bloopLauncher.runFork(args) // run using 'java -cp ...' subprocess
  bloopLauncherClasspath: $.bloopLauncher.classpath // prints the classpath
}
```

## `std.scala.cs(deps, withSources=false)`
Takes an array of `std.scala.Dep` objects and fetches their jars using
coursier. Returns an array of jar paths in classpath order. Fetches source jars
if `withSources` is set to `true`.

## `std.scala.jvm(name, index="https://github.com/coursier/jvm-index/raw/master/index.json")`
Fetches a jvm using coursier.

## `std.scala.compile(project)`
Compiles a `std.scala.Project` object using bloop. Will launch a bloop bsp on
port 8212 if one does not already exist.

## `std.scala.run(project, main, args, jvmOptions, environmentVariables)`
Runs a `std.scala.Project` object using bloop. Will launch a bloop bsp on port
8212 if one does not already exist.


## `std.runJob(desc)`
Takes a job description and runs it, returning a job result object. Jobs are
cached based on file timestamps. e.g.
```jsonnet
std.runJob({
  cmdline: ['java', '-cp', $.bloopLauncher.classpathString, main, args],
  envVars: {
    HOME: std.getenv("HOME"),
    JAVA_HOME: std.getenv("JVM_HOME"),
  },
  inputFiles: [$.bloopLauncher.classpathPaths],
  outputFiles: ['out/foo', 'out/bar'],
})
```