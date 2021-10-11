package root

import bloop.config.Config.{
  CompileOrder,
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
        else path.error(ctx, expr.src, "not a valid")
      }
  given JDecoder[SourcesGlobs] = summon[JDecoder[SourcesGlobs]]
  given JDecoder[Scala] = summon[JDecoder[Scala]]
  given JDecoder[Java] = summon[JDecoder[Java]]
  given JDecoder[Sbt] = summon[JDecoder[Sbt]]
  given JDecoder[Test] = summon[JDecoder[Test]]
  given JDecoder[Platform] = summon[JDecoder[Platform]]
  given JDecoder[Resolution] = summon[JDecoder[Resolution]]

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
  )  derives JDecoder

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
    println("LJKJ")
    val seen = collection.mutable.LinkedHashMap[String, FlatProject]()
    toBloopProject(rproject, seen)
    val res = seen.toList.map(_._2)
    println("skksjLJKJ")
    res

  def write(ctx: EvaluationContext, rproject: RecursiveProject): Seq[java.nio.file.Path] =
    val bloopVersion = "1.4.9"
    toBloopProjects(rproject).map { project =>
      JobRunner.write(ctx.workspaceDir.resolve(project.name), bloop.config.write(
        bloop.config.Config.File(bloopVersion, project)))
    }
