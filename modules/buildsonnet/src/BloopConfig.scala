package buildsonnet

import buildsonnet.evaluator.{EvaluationContext, EvaluatedJValue, JDecoder, JDecoderPath}

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

import cats.{Monad, Parallel}
import cats.effect.Sync
import cats.syntax.all.given

object BloopConfig:
  given [F[_]: Monad]: JDecoder[F, CompileOrder] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[CompileOrder] =
      path.expect[F, String](ctx, expr).flatMap { str =>
        if str == Mixed.id then Mixed.pure
        else if str == JavaThenScala.id then JavaThenScala.pure
        else if str == ScalaThenJava.id then ScalaThenJava.pure
        else path.error(ctx, expr.src, "not a valid compileOrder")
      }

  given [F[_]: Monad]: JDecoder[F, LinkerMode] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[LinkerMode] =
      path.expect[F, String](ctx, expr).flatMap { str =>
        if str == LinkerMode.Debug.id then LinkerMode.Debug.pure
        else if str == LinkerMode.Release.id then LinkerMode.Release.pure
        else path.error(ctx, expr.src, "not a valid linkerMode")
      }

  given [F[_]: Monad]: JDecoder[F, ModuleKindJS] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[ModuleKindJS] =
      path.expect[F, String](ctx, expr).flatMap { str =>
        if str == ModuleKindJS.NoModule.id then ModuleKindJS.NoModule.pure
        else if str == ModuleKindJS.CommonJSModule.id then ModuleKindJS.CommonJSModule.pure
        else if str == ModuleKindJS.ESModule.id then ModuleKindJS.ESModule.pure
        else path.error(ctx, expr.src, "not a valid moduleKind")
      }

  given [F[_]: Monad: Parallel]: JDecoder[F, SourcesGlobs] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Scala] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Java] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Sbt] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Test] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Platform.Js] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Platform.Jvm] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Platform.Native] = JDecoder.derived
  given [F[_]: Monad: Parallel]: JDecoder[F, Platform] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[Platform] =
      path.expect[F, EvaluatedJValue.JObject[F]](ctx, expr).flatMap { obj =>
        for
          name <- obj
            .lookupOpt("name")
            .fold(path.error(ctx, expr.src, "platform must have a 'name' field"))(_.value)
          res <- path.withField("name").expect[F, String](ctx, name).flatMap { name =>
            if name == Platform.Js.name then
              summon[JDecoder[F, Platform.Js]].decode(ctx, path, obj).widen
            else if name == Platform.Jvm.name then
              summon[JDecoder[F, Platform.Jvm]].decode(ctx, path, obj).widen
            else if name == Platform.Native.name then
              summon[JDecoder[F, Platform.Native]].decode(ctx, path, obj).widen
            else
              path.error(
                ctx,
                expr.src,
                s"invalid platform name '$name', expected ${Platform.Js.name}, ${Platform.Jvm.name}, or ${Platform.Native.name}",
              )
          }
        yield res
      }
  given [F[_]: Monad: Parallel]: JDecoder[F, Resolution] = JDecoder.derived

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
    tags: Option[List[String]],
  )
  given [F[_]: Monad: Parallel]: JDecoder[F, RecursiveProject] = JDecoder.derived

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

  def write[F[_]: Sync](
    ctx: EvaluationContext[F],
    rproject: RecursiveProject,
  ): F[List[java.nio.file.Path]] =
    val bloopVersion = "1.4.9"
    toBloopProjects(rproject).traverse { project =>
      Std.writeImpl(
        ctx.workspaceDir.resolve(s".bloop/${project.name}.json"),
        bloop.config.write(bloop.config.Config.File(bloopVersion, project)),
      )
    }
