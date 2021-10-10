package root

import bloop.config.Config.{
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
