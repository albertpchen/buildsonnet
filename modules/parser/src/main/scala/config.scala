package root

import bloop.config.Config.{
  CompileOrder,
  LinkerMode,
  ModuleKindJS,
  Mixed,
  JavaThenScala,
  ScalaThenJava,
  SourcesGlobs,
  Scala,
  Java,
  Sbt,
  Test,
  Platform,
  Resolution,
  Project => FlatProject,
}
import bloop.config.PlatformFiles.Path

import monix.eval.Task

object Config:
  given JDecoder[CompileOrder] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[CompileOrder] =
      path.expectType[EvaluatedJValue.JString](ctx, expr).map { str =>
        if str.str == Mixed.id then Mixed
        else if str.str == JavaThenScala.id then JavaThenScala
        else if str.str == ScalaThenJava.id then ScalaThenJava
        else path.error(ctx, expr.src, "not a valid compileOrder")
      }
  given JDecoder[LinkerMode] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[LinkerMode] =
      path.expectType[EvaluatedJValue.JString](ctx, expr).map { str =>
        if str.str == LinkerMode.Debug.id then LinkerMode.Debug
        else if str.str == LinkerMode.Release.id then LinkerMode.Release
        else path.error(ctx, expr.src, "not a valid linkerMode")
      }
  given JDecoder[ModuleKindJS] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[ModuleKindJS] =
      path.expectType[EvaluatedJValue.JString](ctx, expr).map { str =>
        if str.str == ModuleKindJS.NoModule.id then ModuleKindJS.NoModule
        else if str.str == ModuleKindJS.CommonJSModule.id then ModuleKindJS.CommonJSModule
        else if str.str == ModuleKindJS.ESModule.id then ModuleKindJS.ESModule
        else path.error(ctx, expr.src, "not a valid moduleKind")
      }
  given JDecoder[SourcesGlobs] = JDecoder.derived
  given JDecoder[Scala] = JDecoder.derived
  given JDecoder[Java] = JDecoder.derived
  given JDecoder[Sbt] = JDecoder.derived
  given JDecoder[Test] = JDecoder.derived
  given JDecoder[Platform.Js] = JDecoder.derived
  given JDecoder[Platform.Jvm] = JDecoder.derived
  given JDecoder[Platform.Native] = JDecoder.derived
  given JDecoder[Platform] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[Platform] =
      path.expectType[EvaluatedJValue.JObject](ctx, expr).flatMap { obj =>
        val name = obj.members().getOrElse("name", path.error(ctx, expr.src, "platform must have a 'name' field")).evaluated
        path.withField("name").expectType[EvaluatedJValue.JString](ctx, name).flatMap { name =>
          if name.str == Platform.Js.name then summon[JDecoder[Platform.Js]].decode(ctx, path, obj)
          else if name.str == Platform.Jvm.name then summon[JDecoder[Platform.Jvm]].decode(ctx, path, obj)
          else if name.str == Platform.Native.name then summon[JDecoder[Platform.Native]].decode(ctx, path, obj)
          else path.error(ctx, expr.src,
            s"invalid platform name '${name.str}', expected ${Platform.Js.name}, ${Platform.Jvm.name}, or ${Platform.Native.name}")
        }
      }
  given JDecoder[Resolution] = JDecoder.derived

  case class RecursiveProject(
    name: String,
    directory: Path,
    workspaceDir: Option[Path],
    sources: List[Path],
    sourcesGlobs: Option[List[SourcesGlobs]],
    sourceRoots: Option[List[Path]],
    dependencies: List[RecursiveProject],
    classpath: List[Path],
    out: Path,
    classesDir: Path,
    resources: Option[List[Path]],
    `scala`: Option[Scala],
    java: Option[Java],
    sbt: Option[Sbt],
    test: Option[Test],
    platform: Option[Platform],
    resolution: Option[Resolution],
    tags: Option[List[String]]
  )
  given JDecoder[RecursiveProject] = JDecoder.derived

  private def convert(rproject: RecursiveProject): FlatProject =
    FlatProject(
      name = rproject.name,
      directory = rproject.directory,
      workspaceDir = rproject.workspaceDir,
      sources = rproject.sources,
      sourcesGlobs = rproject.sourcesGlobs,
      sourceRoots = rproject.sourceRoots,
      dependencies = rproject.dependencies.map(_.name),
      classpath = rproject.classpath,
      out = rproject.out,
      classesDir = rproject.classesDir,
      resources = rproject.resources,
      `scala` = rproject.`scala`,
      java = rproject.java,
      sbt = rproject.sbt,
      test = rproject.test,
      platform = rproject.platform,
      resolution = rproject.resolution,
      tags = rproject.tags,
    )

  private def toBloopProject(
    rproject: RecursiveProject,
    seen: collection.mutable.LinkedHashMap[String, FlatProject],
  ): Unit =
    seen.getOrElseUpdate(rproject.name, convert(rproject))
    rproject.dependencies.foreach(toBloopProject(_, seen))

  def toBloopProjects(rproject: RecursiveProject): List[FlatProject] =
    val seen = collection.mutable.LinkedHashMap[String, FlatProject]()
    toBloopProject(rproject, seen)
    seen.toList.map(_._2)

  def write(ctx: EvaluationContext, rproject: RecursiveProject): Seq[java.nio.file.Path] =
    val bloopVersion = "1.4.9"
    toBloopProjects(rproject).map { project =>
      JobRunner.write(ctx.workspaceDir.resolve(s".bloop/${project.name}.json"), bloop.config.write(
        bloop.config.Config.File(bloopVersion, project)))
    }
