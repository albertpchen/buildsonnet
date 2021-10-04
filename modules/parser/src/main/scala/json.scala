package root

import scala.concurrent.{Await, ExecutionContext, Future}

import cats.parse.{Parser0 => P0, Parser => P, Numbers}
import cats.syntax.all._

object Json:
  val a = 0
  import cats.instances.all._
  import JValue._

  extension [T](p: P[T])
    def withRange: P[(Source, T)] =
      (P.index.with1 ~ p ~ P.index).map { case ((start, t), end) =>
        (Source(start, end), t)
      }

  val whitespace: P[Unit] = P.oneOf(List(
    P.charIn(" \t\r\n").void,
    P.char('#') *> P.charsWhile0(_ != '\n').void,
    (P.string("//") *> P.charsWhile0(_ != '\n').void).backtrack,
    (P.string("/*") *> P.oneOf(List(
      P.charWhere(_ != '*'),
      (P.char('*') *> !P.char('/')).backtrack,
    )).rep0 *> P.string("*/")),
  ))
  val whitespaces0, __ = whitespace.rep0.void
  val whitespaces: P[Unit] = whitespace.rep.void

  val idTail = P.charIn('_' +: ('a' to 'z') ++: ('A' to 'Z') ++: ('0' to '9'))
  def keyword(str: String): P[Source] =
    (
      P.string(str).void.withContext(s"expected keyword $str") ~
      !idTail.withContext(s"expected keyword $str")
    ).withRange.map(_._1)
  val keywords = Set(
    "assert", "else", "error", "false", "for", "function", "if", "import", "importstr",
    "in", "local", "null", "tailstrict", "then", "self", "super", "true"
  )
  val id: P[String] =
    P.not(P.stringIn(keywords) *> !idTail)
      .with1
      .*>(P.charIn('_' +: ('a' to 'z') ++: ('A' to 'Z')) ~ idTail.rep0).map { (head, tail) =>
        (head +: tail).mkString
      }

  val pnull = keyword("null").map(JNull(_))
  val ptrue = keyword("true").map(JTrue(_))
  val pfalse = keyword("false").map(JFalse(_))
  val psuper = keyword("super").map(JSuper(_))
  val self = keyword("self").map(JSelf(_))

  val hexDigit = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).map { ch =>
    if ch >= '0' || ch <= '9' then
      ch - '0'
    else if ch >= 'a' || ch <= 'f' then
      ch - 'a' + 10
    else
      ch - 'A' + 10
  }

  val escapedStringChar: P[Char] = P.char('\\') *> P.oneOf(List(
    P.char('"').map(_ => '"'),
    P.char('\'').map(_ => '\''),
    P.char('\\').map(_ => '\\'),
    P.char('/').map(_ => '/'),
    P.char('b').map(_ => '\b'),
    P.char('f').map(_ => '\f'),
    P.char('n').map(_ => '\n'),
    P.char('r').map(_ => '\r'),
    P.char('t').map(_ => '\t'),
    P.char('u') *> (hexDigit, hexDigit, hexDigit, hexDigit).tupled.map { (_1, _2, _3, _4) =>
      (_1.toInt << 12 | _2.toInt << 8 | _3.toInt << 4 | _4.toInt).toChar
    },
  ))
  def stringChar(quote: Char): P[Char] = P.oneOf(List(
    P.charWhere(c => c >= ' ' && c != quote && c != '\\'),
    escapedStringChar,
  ))
  def verbatimStringChar(quote: Char): P[Char] = P.oneOf(List(
    P.charWhere(c => c >= ' ' && c != quote),
    P.char(quote) *> P.char(quote).map(_ => quote),
  ))

  val string = P.oneOf(List(
    P.char('\'') *> stringChar('\'').rep0 <* P.char('\''),
    P.char('"') *> stringChar('"').rep0 <* P.char('"'),
    P.string("@'") *> verbatimStringChar('\'').backtrack.rep0 <* P.char('\''),
    P.string("@\"") *> verbatimStringChar('"').backtrack.rep0 <* P.char('"'),
  )).map(s => s.mkString).withContext("expected string literal")

  val num = (P.index.with1 ~ Numbers.jsonNumber ~ P.index).map {
    case ((beginOff, num), endOff) => JNum(Source(beginOff, endOff), num)
  }
  val listSep: P[Unit] = P.char(',').surroundedBy(__).void
  val dollar = (P.index.with1 <* P.char('$')).map(offset => JOuter(Source(offset, offset + 1)))

  def commaList[A](pa: P[A]): P0[List[A]] = (
    pa.<*(__) ~
    (P.char(',') *> (__) *> pa <* __).backtrack.rep0 <*
    P.char(',').<*(__).?
  ).map { (head, tail) =>
    head +: tail
  }.?.map(_.getOrElse(List.empty))

  var expr: P[JValue] = null
  val exprDefer = P.defer(expr)
  val assert = {
    ((keyword("assert") *> __ *> expr).withRange <* __) ~ (P.char(':') *> __ *> expr).?
  }.map {
    case ((src, cond), msg) => (src, cond, msg)
  }
  val ifspec = keyword("if") *> __ *> exprDefer
  val forspec = (keyword("for") *> id.surroundedBy(__) <* keyword("in")) ~ (__.with1 *> exprDefer)
  val params: P0[List[(String, Option[JValue])]] = P.defer0 {
    P.char('(') *> commaList(id.<*(__) ~ (P.char('=') *> __ *> expr).?) <* P.char(')')
  }
  val bind: P[(Int, String, Option[JParamList], JValue)] = {
    (
      P.index.with1 ~ id.<*(__),
      params.?.with1 <*(P.char('=').surroundedBy(__)),
      exprDefer,
    ).tupled.map { case ((start, name), params, value) =>
      (start, name, params, value)
    }
  }
  val objInside: P0[Source => JValue] = P.defer0 {
    val bindLocal: P[JObjMember.JLocal] = (keyword("local") *> whitespaces *> bind).map { (start, id, paramsOpt, expr) =>
      JObjMember.JLocal(expr.src.withStart(start), id, paramsOpt.fold(expr)(JFunction(expr.src.withStart(start), _, expr)))
    }
    val objComp = (
      (bindLocal <* __ <* P.char(',') <* __).backtrack.rep0,
      __ *> P.char('[') *> __ *> expr <* __ <* P.char(']') <* __,
      P.char(':') *> __ *> expr <* __,
      (P.char(',') *> __ *> bindLocal <* __).backtrack.rep0,
      __ *> P.char(',').? *> __ *> forspec <* __,
      ifspec.?,
    ).tupled.map { case (preLocals, key, value, postLocals, (inId, inExpr), cond) =>
      JObjectComprehension(_, preLocals, key, value, postLocals, inId, inExpr, cond)
    }
    val key: P[JValue] = P.oneOf(List(
      string.withRange.map(JString(_, _)),
      id.withRange.map(JString(_, _)),
      (P.char('[') *> __ *> expr <* __ <* P.char(']')),
    ))
    val h = (P.char('+').?.with1 ~ P.char(':') ~ P.char(':').?  ~ P.char(':').?).map { case (((plus, _1), _2), _3) =>
      plus.isDefined -> (_2.isDefined || _3.isDefined)
    }
    val keyValue: P[JObjMember] = {
      P.oneOf(List(
        (P.index.with1 ~ key, h.surroundedBy(__), expr).tupled.map { case ((start, key), (plus, isHidden), value) =>
          JObjMember.JField(value.src.withStart(start), key, plus, isHidden, value)
        },
        assert.map(JObjMember.JAssert(_, _, _)),
        (keyword("local") *> __ *> bind).map { (start, id, paramsOpt, expr) =>
          JObjMember.JLocal(expr.src.withStart(start), id, paramsOpt.fold(expr)(JFunction(expr.src.withStart(start), _, expr)))
        },
      ))
    }
    P.oneOf0(List(
      objComp.backtrack,
      commaList(keyValue).map(members => JValue.JObject(_, members)),
    ))
  }

  val listComp = (
    exprDefer,
    forspec.surroundedBy(__) ~ ifspec.?,
  ).tupled.map { case (forExpr, ((forVar, inExpr), cond)) =>
    JArrayComprehension(_, forVar, forExpr, inExpr, cond)
  }
  val exprBase: P[JValue] = {
    val list =
      P.oneOf0(List(
        listComp.backtrack,
        commaList(exprDefer).map(elements => JArray(_, elements)),
      ))
        .surroundedBy(__)
        .with1
        .between(P.char('['), P.char(']'))
        .withRange
        .map((src, fn) => fn(src))

    val function = (keyword("function") *> __ *> (params <* __) ~ exprDefer).withRange.map {
      case (range, (params, body)) => JFunction(range, params, body)
    }
    val ifExpr = (
      (P.index.with1 ~ (keyword("if") *> __ *> exprDefer <* __)) ~
      (keyword("then") *> __ *> exprDefer <* __) ~
      (keyword("else") *> __ *> exprDefer).?
    ).map { case (((startIdx, cond), trueValue), elseValue) =>
      val src = elseValue.getOrElse(trueValue).src.withStart(startIdx)
      JIf(src, cond, trueValue, elseValue)
    }
    val obj =
      (P.char('{') *> __ *> objInside <* __ <* P.char('}'))
        .withRange
        .map((src, fn) => fn(src))
    val grouped = P.char('(') *> __ *> exprDefer <* __ <* P.char(')')
    val local = (
      keyword("local") *> __ *> bind.repSep(__ *> P.char(',') *> __) ~
      (__ *> P.char(';') *> __ *> exprDefer)
    ).withRange.map { case (src, (binds, result)) =>
      binds.toList.foldRight(result) { case ((start, id, paramsOpt, expr), acc) =>
        JLocal(src, id, paramsOpt.fold(expr)(JFunction(expr.src.withStart(start), _, expr)), acc)
      }
    }
    val error = (keyword("error") *> __ *> exprDefer).withRange.map(JError(_, _))
    val importExpr = (keyword("import") *> __ *> string).withRange.map(JImport(_, _))
    val importStrExpr = (keyword("importstr") *> __ *> string).withRange.map(JImportStr(_, _))
    val assertExp = ((assert <* __ <* P.char(';') <* __) ~ exprDefer).map { case ((src, cond, msg), result) =>
      JAssert(src, cond, msg, result)
    }
    P.oneOf(List(
      string.withRange.map(JString(_, _)),
      num,
      list,
      obj,
      grouped,
      pnull.backtrack,
      pfalse.backtrack,
      ptrue.backtrack,
      self.backtrack,
      psuper.backtrack,
      dollar,
      ifExpr,
      function.backtrack,
      local.backtrack,
      assertExp.backtrack,
      importExpr.backtrack,
      importStrExpr.backtrack,
      id.withRange.map(JId(_, _)),
      error,
    ))
  }.withContext("expected expression")

  val op = P.stringIn(Array(
    "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
    "*", "/", "%", "+", "-", "<", ">", "&", "^", "|", "in",
  )).map(JBinaryOperator(_))

  val args: P0[List[(Option[String], JValue)]] = {
    val argName = (id <* __ <* P.char('=')).backtrack.?
    commaList(argName.<*(__).with1 ~ exprDefer)
  }

  val unaryOp = P.charIn("+-!~")
  val exprAtom: P[JValue] = {
    val suffix: P[JValue => JValue] =
      P.index
        .with1
        .~(P.charIn(Array('.', '(', '[', '{')))
        .<*(__)
        .flatMap {
          case (_, '.') => (id ~ P.index).map { (field, end) =>
            (jv: JValue) => JGetField(jv.src.withEnd(end), jv, field)
          }
          case (_, '[') =>
            val suffix = P.char(':') *> __ *> exprDefer.? <* __
            (
              (exprDefer <* __) ~ (suffix ~ suffix.?).?.<*(P.char(']')) ~ P.index
            ).map { case ((index, opt), end) =>
              if opt.isEmpty then
                (jv: JValue) => JIndex(jv.src.withEnd(end), jv, index)
              else
                val (endIndex, strideOpt) = opt.get
                (jv: JValue) => JSlice(jv.src.withEnd(end), jv, index, endIndex, strideOpt.flatten)
            }
          case (_, '(') => ((args <* __  <* P.char(')')) ~ P.index).flatMap { (args, end) =>
            val namedAfterPositional = args.size >= 2 && args.sliding(2).exists { window =>
              val Seq((first, _), (second, _)) = window
              first.isDefined && second.isEmpty
            }
            if namedAfterPositional then
              P.failWith("Positional argument after a named argument is not allowed")
            else
              val (positional, named) = args.partition(_._1.isEmpty)
              P.pure((jv: JValue) => JApply(jv.src.withEnd(end), jv, positional.map(_._2), named.map((id, expr) => id.get -> expr)))
          }
          case (start, '{') => ((objInside <* __  <* P.char('}')) ~ P.index).map { (objInside, end) =>
            (jv: JValue) => JBinaryOp(
              jv.src.withEnd(end),
              jv,
              JBinaryOperator.Op_+,
              objInside(Source(start, end)),
            )
          }
        }
        <* __
    ((unaryOp.? <* __).with1 ~ exprBase.<*(__) ~ suffix.rep0).map { case ((unaryOp, base), fns) =>
      val expr = fns.foldLeft(base) { (base, fn) => fn(base) }
      unaryOp match
        case None => expr
        case Some(op) => JUnaryOp(Source.Generated, JUnaryOperator(op), expr)
    }
  }

  expr = {
    (exprAtom.<*(__) ~ (op.surroundedBy(__) ~ exprAtom).rep0).map { (head, tail) =>
      val tailSize = tail.size
      var exprs = tail
      def climb(curr: JValue, minPrec: Int): JValue =
        var result = curr
        while
          if exprs.isEmpty then
            false
          else
            val (op, expr) = exprs.head
            val cond = exprs.head._1.precedence >= minPrec
            if cond then
              val nextPrec = if op.isLeftAssociative then minPrec + 1 else minPrec
              exprs = exprs.tail
              val rhs = climb(expr, nextPrec)
              result = JBinaryOp(result.src.merge(rhs.src), result, op, rhs)
            cond
        do ()
        result
      climb(head, 0)
    }
  }
  val parserFile = __ *> expr <* __

  
trait Cancelable {
  def cancel(): Unit
}

object Cancelable {
  def apply(fn: () => Unit): Cancelable =
    new Cancelable {
      override def cancel(): Unit = fn()
    }
  val empty: Cancelable = Cancelable(() => ())
}
  
import scala.concurrent.{ExecutionContextExecutorService, Promise}
case class SocketConnection(
  serverName: String,
  output: java.io.OutputStream,
  input: java.io.InputStream,
  cancelables: List[Cancelable],
  finishedPromise: Promise[Unit]
)

import java.nio.channels.Channels
import java.nio.channels.Pipe
import bloop.launcher.LauncherMain
def connectToLauncher(
  name: String,
  bloopVersion: String,
  bloopPort: Int,
)(using ec: ExecutionContextExecutorService): Future[SocketConnection] = {
  val launcherInOutPipe = Pipe.open()
  // val launcherIn = new QuietInputStream(
  //   Channels.newInputStream(launcherInOutPipe.source()),
  //   "Bloop InputStream"
  // )
  val launcherIn = Channels.newInputStream(launcherInOutPipe.source())
  // val clientOut = new ClosableOutputStream(
  //   Channels.newOutputStream(launcherInOutPipe.sink()),
  //   "Bloop OutputStream"
  // )
  val clientOut = Channels.newOutputStream(launcherInOutPipe.sink())

  val clientInOutPipe = Pipe.open()
  val clientIn = Channels.newInputStream(clientInOutPipe.source())
  val launcherOut = Channels.newOutputStream(clientInOutPipe.sink())

  println("111")
  val serverStarted = Promise[Unit]()
  val launcher =
    new LauncherMain(
      launcherIn,
      launcherOut,
      System.err,
      java.nio.charset.StandardCharsets.UTF_8,
      bloop.bloopgun.core.Shell.default,
      userNailgunHost = None,
      userNailgunPort = Some(bloopPort),
      serverStarted
    )
  println("222")

  val finished = Promise[Unit]()
  val job = ec.submit(new Runnable {
    override def run(): Unit = {
        println("666")
      launcher.runLauncher(
        bloopVersion,
        skipBspConnection = false,
        Nil
      )
      println("777")
      finished.success(())
    }
  })
  println("333")

  serverStarted.future.map { _ =>
    println("444")
    SocketConnection(
      name,
      clientOut,
      clientIn,
      List(
        Cancelable { () =>
          println("555")
          clientOut.flush()
          clientOut.close()
        },
        Cancelable(() => job.cancel(true))
      ),
      finished
    )
  }
}

def bootstrap()(using ExecutionContext): Future[Unit] =
  import coursier.{Dependency, Fetch, Module, ModuleName, Organization}
  import coursier.cache.FileCache
  import coursier.cache.loggers.RefreshLogger
  Fetch()
    .withDependencies(Seq(
      CoursierDependency("ch.epfl.scala", "bloop-launcher_2.12", "1.4.9").toDependency
    ))
    .withCache(
      FileCache().withLogger(RefreshLogger.create(System.out))
    )
    .futureResult()
    .map { result =>
      val (asFiles, asUrls) = result.artifacts.partition {
        case (a, _) => a.url.startsWith("file:")
      }
      val urls0 = asUrls.map(_._1.url).map(coursier.launcher.ClassPathEntry.Url(_))
      val files0 = asFiles.map(_._2).map { f =>
        coursier.launcher.ClassPathEntry.Resource(
          f.getName,
          f.lastModified(),
          java.nio.file.Files.readAllBytes(f.toPath)
        )
      }
      val content = coursier.launcher.ClassLoaderContent(urls0 ++ files0)

      val bootstrap = coursier.launcher.Parameters.Bootstrap(Seq(content), "bloop.launcher.LauncherMain")
        .withJavaProperties(Seq.empty)
        .withDeterministic(false)
        .withPreambleOpt(Some(
          coursier.launcher.Preamble()
        ))
        .withProguarded(true)
        .withHybridAssembly(false)
        .withDisableJarChecking(None)
        .withPython(false)
      coursier.launcher.Generator.generate(bootstrap, java.nio.file.Paths.get("buildsonnet-bootstrap"))
    }

import ch.epfl.scala.bsp4j.{
  BuildClient,
  BuildServer,
  BuildClientCapabilities,
  BuildTargetIdentifier,
  CompileParams,
  InitializeBuildParams,
  ScalaBuildServer,
  ShowMessageParams,
  LogMessageParams,
  TaskStartParams,
  TaskProgressParams,
  TaskFinishParams,
  PublishDiagnosticsParams,
  DidChangeBuildTarget,
}
trait BloopServer extends BuildServer with ScalaBuildServer

def client(
  workspace: java.nio.file.Path,
  connection: SocketConnection,
)(using ec: ExecutionContextExecutorService): Future[Unit] =
  import java.util.concurrent.{Future => _, _}
  import org.eclipse.lsp4j.jsonrpc.Launcher

  val localClient = new BuildClient:
    def afterBuildTaskFinish(bti: String) =
      println(s"afterBuildTaskFinish: $bti")

    override def onBuildShowMessage(params: ShowMessageParams): Unit =
      println(s"onBuildShowMessage: $params")

    override def onBuildLogMessage(params: LogMessageParams): Unit =
      println(s"onBuildLogMessage: $params")

    override def onBuildTaskStart(params: TaskStartParams): Unit =
      println(s"onBuildTaskStart: $params")

    override def onBuildTaskProgress(params: TaskProgressParams): Unit =
      println(s"onBuildTaskProgress: $params")

    override def onBuildTaskFinish(params: TaskFinishParams): Unit =
      println(s"onBuildTaskFinish: params")

    override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit =
      println(s"onBuildPublishDiagnostics: $params")

    override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit =
      println(s"onBuildTargetDidChange: $params")

  val launcher = new Launcher.Builder[BloopServer]()
    .setOutput(connection.output)
    .setInput(connection.input)
    .setLocalService(localClient)
    .setExecutorService(ec)
    .setRemoteInterface(classOf[BloopServer])
    .create()
  println("launch 111")
  val server = launcher.getRemoteProxy
  println("launch 222")
  localClient.onConnectWithServer(server)
  println("launch 333")
  // val luancherListener = ec.submit(new Runnable {
  //   override def run() = launcher.startListening().get()
  // })
  val listening = launcher.startListening()
  println("launch 444")
  import scala.jdk.FutureConverters.given
  val initializeResult = server.buildInitialize(new InitializeBuildParams(
    "buildsonnet", // name of this client
    "0.0.1", // version of this client
    "1.0.0", // BSP version
    workspace.toUri().toString(),
    new BuildClientCapabilities(java.util.Collections.singletonList("scala"))
  ))
  println("launch 555")
  import scala.jdk.CollectionConverters.given
  for
    _ <- initializeResult.asScala
    _ = println("init 111")
    _ = server.onBuildInitialized()
    _ = println("init 222")
    compileResult <- server.buildTargetCompile(
      new CompileParams(Seq(new BuildTargetIdentifier(s"file://$workspace/?id=parser")).asJava)
    ).asScala
    _ = println(s"SLDFJLDSKJF: $compileResult")
    _ = println("init 333")
    _ <- server.buildShutdown().asScala
    _ = println("init 444")
  yield
    server.onBuildExit()
    println("init 555")
    connection.finishedPromise.success(())
    println("init 666")
       

@main
def asdf(args: String*): Unit =
  import scala.concurrent.duration.Duration
  import scala.util.{Failure, Success}
  val filename = args(0)
  val source = scala.io.Source.fromFile(filename).getLines.mkString("\n")
  val sourceFile = SourceFile(filename, source)
  val parser = Json.parserFile


  val exec = java.util.concurrent.Executors.newCachedThreadPool()
  given ExecutionContextExecutorService = ExecutionContext.fromExecutorService(exec)
  //Await.result(bootstrap(), Duration.Inf)
  println("launching bloop")
  val bloopPort = 8218
  val socketConnection = Await.result(connectToLauncher("buildsonnet", "1.4.9", bloopPort), Duration.Inf)
  println("launched bloop")
  println("closing bloop")
  Await.result(client(java.nio.file.Paths.get(".").toAbsolutePath.normalize, socketConnection), Duration.Inf)
  //socketConnection.cancelables.foreach(_.cancel())
  //socketConnection.input.close()
  println("closed bloop")
  println("START")
  parser.parseAll(source).fold(
    error => {
      println("END")
      println("FAIL: " + error.toString)
    },
    ast => {
      println("END PARSE")
      val ctx = EvaluationContext(sourceFile).bindEvaluated("std", Std.obj)
      val manifested = manifest(ctx)(ast)
      manifested.fold(
        msg => println(s"FAIL: $msg"),
        value => println(value),
      )
    }
  )
  summon[ExecutionContextExecutorService].shutdownNow()

/*
Warning: Dynamic proxy method java.lang.reflect.Proxy.newProxyInstance invoked at org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints.toServiceObject(ServiceEndpoints.java:41)
Warning: Dynamic proxy method java.lang.reflect.Proxy.newProxyInstance invoked at com.sun.jna.Native.loadLibrary(Native.java:648)
Warning: Dynamic proxy method java.lang.reflect.Proxy.newProxyInstance invoked at org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints.toServiceObject(ServiceEndpoints.java:54)
Warning: Aborting stand-alone image build due to dynamic proxy use without configuration.
Warning: Use -H:+ReportExceptionStackTraces to print stacktrace of underlying exception
*/
