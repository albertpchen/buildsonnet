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

object Config:
  given JDecoder[CompileOrder] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): concurrent.Future[CompileOrder] =
      given concurrent.ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JString](ctx, expr).map { str =>
        if str.str == Mixed.id then Mixed
        else if str.str == JavaThenScala.id then JavaThenScala
        else if str.str == ScalaThenJava.id then ScalaThenJava
        else path.error(ctx, expr.src, "not a valid compileOrder")
      }
  given JDecoder[LinkerMode] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): concurrent.Future[LinkerMode] =
      given concurrent.ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JString](ctx, expr).map { str =>
        if str.str == LinkerMode.Debug.id then LinkerMode.Debug
        else if str.str == LinkerMode.Release.id then LinkerMode.Release
        else path.error(ctx, expr.src, "not a valid linkerMode")
      }
  given JDecoder[ModuleKindJS] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): concurrent.Future[ModuleKindJS] =
      given concurrent.ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JString](ctx, expr).map { str =>
        if str.str == ModuleKindJS.NoModule.id then ModuleKindJS.NoModule
        else if str.str == ModuleKindJS.CommonJSModule.id then ModuleKindJS.CommonJSModule
        else if str.str == ModuleKindJS.ESModule.id then ModuleKindJS.ESModule
        else path.error(ctx, expr.src, "not a valid moduleKind")
      }
  implicit val a: JDecoder[SourcesGlobs] = JDecoder.derived[SourcesGlobs]
  implicit val b: JDecoder[Scala] = JDecoder.derived[Scala]
  implicit val c: JDecoder[Java] = JDecoder.derived[Java]
  implicit val d: JDecoder[Sbt] = JDecoder.derived[Sbt]
  implicit val e: JDecoder[Test] = JDecoder.derived[Test]
  implicit val f: JDecoder[Platform.Js] = JDecoder.derived[Platform.Js]
  implicit val g: JDecoder[Platform.Jvm] = JDecoder.derived[Platform.Jvm]
  implicit val h: JDecoder[Platform.Native] = JDecoder.derived[Platform.Native]
  implicit val i: JDecoder[Platform] = new JDecoder[Platform]:
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): concurrent.Future[Platform] =
      given concurrent.ExecutionContext = ctx.executionContext
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
  implicit val j: JDecoder[Resolution] = JDecoder.derived[Resolution]

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
  implicit val k: JDecoder[RecursiveProject] = JDecoder.derived[RecursiveProject]

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
      JobRunner.write(ctx.workspaceDir.resolve(project.name), bloop.config.write(
        bloop.config.Config.File(bloopVersion, project)))
    }
