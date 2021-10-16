package root

import monix.eval.Task

import scala.concurrent.{ExecutionContext, Future}
import cats.data.Nested

sealed trait Importer:
  def `import`(ctx: EvaluationContext, src: Source, fileName: String): EvaluatedJValue
  def importStr(ctx: EvaluationContext, src: Source, fileName: String): EvaluatedJValue.JString

object Importer:
  private[root] val std: Importer = new Importer:
    def `import`(ctx: EvaluationContext, src: Source, fileName: String) = throw new Exception("internal error, std context import should never be called")
    def importStr(ctx: EvaluationContext, src: Source, fileName: String) = throw new Exception("internal error, std context importStr should never be called")

  def apply(): Importer = new Importer:
    private val cache = new collection.concurrent.TrieMap[String, EvaluatedJValue]()
    private val strCache = new collection.concurrent.TrieMap[String, String]()
    def `import`(ctx: EvaluationContext, src: Source, fileName: String): Task[EvaluatedJValue] =
      val currFile = new java.io.File(ctx.file.path)
      val importFile = {
        val currFileParent = currFile.toPath.getParent
        val path =
          if currFileParent eq null then
            new java.io.File(fileName).toPath
          else
            currFileParent.resolve(fileName)
        path.normalize().toFile
      }
      if importFile == currFile then
        ctx.error(src, s"file $importFile imports itself")
      val normalized = importFile.toString
      // TODO: add semaphore in case of concurrent imports of same file
      cache.get(normalized).fold {
        val source = scala.io.Source.fromFile(normalized).getLines.mkString("\n")
        val srcFile = SourceFile(normalized, source)
        Parser(srcFile).parseFile.fold(
          error => {
            val result = ctx.error(src, error.toString)
            cache(normalized) = result
            Task.pure(result)
          },
          ast => {
            given concurrent.ExecutionContextExecutorService = ctx.executionContext
            val sourceFile = SourceFile(normalized, source)
            val withOutStd = EvaluationContext(sourceFile, ctx.workspaceDir, ctx.bloopServer)
            val newCtx = withOutStd.bindEvaluated("std", Std.obj(withOutStd))
            val evaluated = evalUnsafe(newCtx)(ast)
            evaluated.map(cache(normalized) = _)
            evaluated
          }
        )
      } { a => Task.pure(a)
      }

    def importStr(ctx: EvaluationContext, src: Source, fileName: String): EvaluatedJValue.JString =
      val currFile = new java.io.File(ctx.file.path)
      val normalized = currFile.toPath.resolve(fileName).normalize().toFile.toString
      val contents = strCache.getOrElseUpdate(normalized, {
        val source = scala.io.Source.fromFile(normalized).getLines.mkString("\n")
        source
      })
      EvaluatedJValue.JString(src, contents)

sealed trait LazyValue:
  def evaluated: Task[EvaluatedJValue]

sealed trait LazyObjectValue extends LazyValue:
  val isHidden: Boolean
  def withHidden(hidden: Boolean): LazyObjectValue

object LazyValue:
  def apply(ctx: () => EvaluationContext, code: JValue): LazyValue =
    new LazyValue:
      val evaluated = evalUnsafe(ctx())(code).memoize
      override def toString = code.toString

  def apply(ctx: EvaluationContext, code: JValue): LazyValue =
    new LazyValue:
      val evaluated = evalUnsafe(ctx)(code).memoize
      override def toString = code.toString

  def strict(value: EvaluatedJValue): LazyValue =
    new LazyValue:
      val evaluated = Task.pure(value)
      override def toString = value.toString

  def strictObject(value: EvaluatedJValue, hidden: Boolean): LazyObjectValue =
    new LazyObjectValue:
      val isHidden = hidden
      def withHidden(isHidden: Boolean) = strictObject(value, isHidden)
      val evaluated = Task.pure(value)
      override def toString = value.toString

  private def withHiddenImp(newIsHidden: Boolean, lazyVal: LazyObjectValue): LazyObjectValue =
    if newIsHidden ^ lazyVal.isHidden then
      new LazyObjectValue:
        val isHidden = newIsHidden
        def evaluated = lazyVal.evaluated
        def withHidden(isHidden: Boolean) = withHiddenImp(isHidden, this)
        override def toString = lazyVal.toString
    else
      lazyVal

  def apply(ctx: () => EvaluationContext, code: JValue, hidden: Boolean): LazyObjectValue =
    new LazyObjectValue:
      self =>
      val isHidden = hidden
      val evaluated = evalUnsafe(ctx())(code).memoize

      def withHidden(hidden: Boolean) = withHiddenImp(hidden, this)
      override def toString = code.toString

  def apply(ctx: EvaluationContext, code: JValue, hidden: Boolean): LazyObjectValue =
    new LazyObjectValue:
      self =>
      val isHidden = hidden
      val evaluated = evalUnsafe(ctx)(code).memoize

      def withHidden(hidden: Boolean) = withHiddenImp(hidden, this)
      override def toString = code.toString


final class StackEntry(
  val src: Source,
  val message: String,
):
  override def toString: String =
    val srcString = src match
    case g: Source.Generated => g.file.path
    case src: Source.Range =>
      val (startLine, startCol) = src.file.getLineCol(src.start)
      val (endLine, endCol) = src.file.getLineCol(src.end)
      if startLine == endLine then
        s"${src.file.path}:$startLine:$startCol-$endCol"
      else
        s"${src.file.path}:($startLine:$startCol)-($endLine:$endCol)"
    if message.isEmpty then srcString else s"$srcString $message"

object StackEntry:
  def function(src: Source): StackEntry =
    new StackEntry(src, "function")

  def apply(src: Source): StackEntry =
    new StackEntry(src, "")

  def objectField(src: Source): StackEntry =
    new StackEntry(src, "object")


sealed trait EvaluationContext:
  def withScope(scope: Map[String, LazyValue]): EvaluationContext
  def executionContext: concurrent.ExecutionContextExecutorService
  def file: SourceFile
  def withFile(file: SourceFile): EvaluationContext

  def bloopServer: Bsp4sBloopServerConnection

  protected def importer: Importer
  final def `import`(src: Source, fileName: String): EvaluatedJValue =
    importer.`import`(this, src, fileName)
  final def importStr(src: Source, fileName: String): EvaluatedJValue.JString =
    importer.importStr(this, src, fileName)

  def workspaceDir: java.nio.file.Path
  protected def jobRunner: JobRunner
  final def runJob(src: Source, desc: JobDescription): Future[EvaluatedJValue.JJob] =
    jobRunner.run(this, src, desc)

  def error(src: Source, message: String): EvaluatedJValue.JError
  def lookup(src: Source, id: String): LazyValue
  def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext
  def functionCtx(fn: Source): EvaluationContext
  def bind(id: String, value: JValue): EvaluationContext
  def bindEvaluated(id: String, value: EvaluatedJValue): EvaluationContext
  def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue): EvaluationContext
  def self(src: Source): EvaluatedJValue.JObject
  def `super`(src: Source): EvaluatedJValue.JObject
  def hasSuper: Boolean
  def withStackEntry(entry: StackEntry): EvaluationContext

sealed trait ObjectEvaluationContext extends EvaluationContext:
  def bind(id: String, value: JValue): ObjectEvaluationContext
  def bindEvaluated(id: String, value: EvaluatedJValue): ObjectEvaluationContext
  def withSelf(self: EvaluatedJValue.JObject): ObjectEvaluationContext
  def withSuper(self: EvaluatedJValue.JObject): ObjectEvaluationContext
  def withStackEntry(entry: StackEntry): ObjectEvaluationContext
  def superChain: collection.immutable.Queue[EvaluatedJValue.JObject]

given theExpr(using expr: EvaluatedJValue): EvaluatedJValue = expr

object EvaluationContext:

  inline def typeString[T]: String =
    val types = macros.mapUnionType[T, String] {
      case _: EvaluatedJValue.JBoolean => "boolean"
      case _: EvaluatedJValue.JNull => "null"
      case _: EvaluatedJValue.JString => "string"
      case _: EvaluatedJValue.JNum => "number"
      case _: EvaluatedJValue.JJob => "job"
      case _: EvaluatedJValue.JPath => "path"
      case _: EvaluatedJValue.JArray => "array"
      case _: EvaluatedJValue.JObject => "object"
      case _: EvaluatedJValue.JFunction => "function"
    }
    if types.size == 1 then
      types.head
    else if types.size == 2 then
      s"${types(0)} or ${types(1)}"
    else
      types.reverse.tail.fold(s"or ${types.last}") {
        (str, tpe) => s"$tpe, $str"
      }

  def typeString(expr: EvaluatedJValue): String =
    expr match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull => "null"
    case _: EvaluatedJValue.JString => "string"
    case _: EvaluatedJValue.JNum => "number"
    case _: EvaluatedJValue.JJob => "job"
    case _: EvaluatedJValue.JPath => "path"
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"

  extension (ctx: EvaluationContext)
    def decode[T: JDecoder](expr: EvaluatedJValue): TypedJValueTask[T] = JDecoder[T].decode(ctx, expr)

    inline def expectBoolean(code: JValue): Nested[Task, TypedJValue, Boolean] = expectType[Boolean](code)
    inline def expectBoolean(expr: EvaluatedJValue): TypedJValue[Boolean] = expectType[Boolean](expr)

    inline def expectNum(code: JValue): Nested[Task, TypedJValue, Double] = expectType[Double](code)
    inline def expectNum(expr: EvaluatedJValue): TypedJValue[Double] = expectType[Double](expr)

    inline def expectString(code: JValue): Nested[Task, TypedJValue, String] = expectType[String](code)
    inline def expectString(expr: EvaluatedJValue): TypedJValue[String] = expectType[String](expr)

    inline def expectArray(code: JValue): Nested[Task, TypedJValue, Seq[EvaluatedJValue]] = expectType[Seq[EvaluatedJValue]](code)
    inline def expectArray(expr: EvaluatedJValue): TypedJValue[Seq[EvaluatedJValue]] = expectType[Seq[EvaluatedJValue]](expr)

    inline def expectObject(code: JValue): Nested[Task, TypedJValue, EvaluatedJObject] = expectType[EvaluatedJObject](code)
    inline def expectObject(expr: EvaluatedJValue): TypedJValue[EvaluatedJObject] = expectType[EvaluatedJObject](expr)

    inline def expectFunction(code: JValue): Nested[Task, TypedJValue, EvaluatedJValue.JFunction] = expectType[EvaluatedJValue.JFunction](code)
    inline def expectFunction(expr: EvaluatedJValue): TypedJValue[EvaluatedJValue.JFunction] = expectType[EvaluatedJValue.JFunction](expr)

    inline def expectFieldName(code: JValue): Nested[Task, TypedJValue, String | Unit] =
      Nested(evalUnsafe(ctx)(code).map(
        expectType[String | Unit](_, s"Field name must be string or null, got ${typeString(theExpr)}")
      ))

    inline def expectType[T](expr: EvaluatedJValue, msg: EvaluatedJValue ?=> String): TypedJValue[T] =
      given ExecutionContext = ctx.executionContext
      macros.typedUnionApply[T, TypedJValue[T]](expr, {
        case _: Boolean => (v: EvaluatedJValue.JBoolean) => TypedJValue(v.value)
        case _: String  => (v: EvaluatedJValue.JString) => TypedJValue(v.str)
        case _: Double  => (v: EvaluatedJValue.JNum) => TypedJValue(v.double)
        case _: Seq[EvaluatedJValue]  => (v: EvaluatedJValue.JArray) => TypedJValue(v.elements)
        case _: EvaluatedJObject  => (v: EvaluatedJValue.JObject) => TypedJValue(v.imp)
        case _: Unit  => (v: EvaluatedJValue.JNull) => TypedJValue(())
        case _ => TypedJValue.error(ctx.error(expr.src, msg(using expr)))
      })

    inline def expectType[T](expr: EvaluatedJValue): TypedJValue[T] =
      expectType[T](expr, s"Unexpected type ${typeString(theExpr)}, expected ${typeString[T]}")

    inline def expectType[T](code: JValue): Nested[Task, TypedJValue, T] =
      Nested(evalUnsafe(ctx)(code).map(expectType[T](_)))

  private[root] case class Imp(
    val bloopServer: Bsp4sBloopServerConnection,
    val importer: Importer,
    val jobRunner: JobRunner,
    val workspaceDir: java.nio.file.Path,
    file: SourceFile,
    scope: Map[String, LazyValue],
    stack: List[StackEntry],
    executionContext: concurrent.ExecutionContextExecutorService,
  ) extends EvaluationContext:
    def error(src: Source, message: String): EvaluatedJValue.JError =
      EvaluatedJValue.JError(src, message, stack)

    def lookup(src: Source, id: String): LazyValue =
      scope.get(id).getOrElse(
        LazyValue.strict(error(src, s"no variable $id defined"))
      )

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        bloopServer,
        obj,
        collection.immutable.Queue.empty,
        this,
        Map.empty,
        stack,
      ).bind("$", JValue.JSelf(Source.Generated(file)))

    def withScope(extraScope: Map[String, LazyValue]): EvaluationContext =
      this.copy(scope = scope ++ extraScope)

    def withFile(newFile: SourceFile): EvaluationContext =
      this.copy(file = newFile)

    def functionCtx(fn: Source) =
      this.copy(stack = StackEntry.function(fn) +: stack)

    def bind(id: String, value: JValue) =
      val newScope = scope + (id -> LazyValue(this, value))
      this.copy(scope = newScope)

    def bindEvaluated(id: String, value: EvaluatedJValue) =
      val newScope = scope + (id -> LazyValue.strict(value))
      this.copy(scope = newScope)

    def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue) =
      val newScope = scope + (id -> LazyValue(ctx, value))
      this.copy(scope = newScope)

    def self(src: Source): EvaluatedJValue.JError | EvaluatedJValue.JObject = error(src, "no self")
    def `super`(src: Source): EvaluatedJValue.JError | EvaluatedJValue.JObject = error(src, "no super")
    def hasSuper: Boolean = false
    def withStackEntry(entry: StackEntry) = this.copy(stack = entry +: stack)

  private[root] case class ObjectImp(
    bloopServer: Bsp4sBloopServerConnection,
    selfObj: EvaluatedJValue.JObject,
    superChain: collection.immutable.Queue[EvaluatedJValue.JObject],
    topCtx: EvaluationContext,
    locals: Map[String, JValue | EvaluatedJValue | LazyValue],
    stack: List[StackEntry],
  ) extends ObjectEvaluationContext:
    export topCtx.{importer, jobRunner, workspaceDir, file, executionContext}

    def self(src: Source) = selfObj
    def `super`(src: Source) = superChain.headOption.getOrElse(topCtx.`super`(src))
    def hasSuper: Boolean = superChain.headOption.isDefined

    def error(src: Source, message: String): EvaluatedJValue.JError =
      EvaluatedJValue.JError(src, message, stack)

    lazy val cache = collection.mutable.HashMap[String, EvaluatedJValue]()
    val scope = locals.map {
      case (id, value: JValue) => id -> LazyValue(this, value)
      case (id, value: EvaluatedJValue) => id -> LazyValue.strict(value)
      case (id, value: LazyValue) => id -> value
    }
    def lookup(src: Source, id: String): LazyValue =
      if scope.contains(id) then
        scope(id)
      else
        topCtx.lookup(src, id)

    def withScope(extraScope: Map[String, LazyValue]): EvaluationContext =
      this.copy(locals = locals ++ extraScope)

    def withFile(newFile: SourceFile): EvaluationContext =
      this.copy(topCtx = topCtx.withFile(newFile))

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        bloopServer,
        obj,
        collection.immutable.Queue.empty,
        topCtx.withScope(scope),
        Map.empty,
        stack,
      )

    def functionCtx(fn: Source) =
      this.copy(stack = StackEntry.function(fn) +: stack)

    def withSelf(newSelf: EvaluatedJValue.JObject) =
      if `self` == newSelf then this else this.copy(selfObj = newSelf)

    def withSuper(parent: EvaluatedJValue.JObject) =
      this.copy(superChain = superChain :+ parent)

    def bind(id: String, value: JValue) =
      this.copy(locals = locals + (id -> value))

    def bindEvaluated(id: String, value: EvaluatedJValue) =
      this.copy(locals = locals + (id -> value))

    def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue) =
      this.copy(locals = locals + (id -> LazyValue(ctx, value)))

    def withStackEntry(entry: StackEntry) =
      this.copy(stack = entry +: stack)

  def apply(
    file: SourceFile,
    workspace: java.nio.file.Path,
    bloopServer: Bsp4sBloopServerConnection,
  )(using executionContext: concurrent.ExecutionContextExecutorService): EvaluationContext =
    Imp(
      bloopServer,
      Importer(),
      JobRunner(),
      workspace,
      file,
      Map.empty,
      List.empty,
      executionContext
    )

  def apply(
    file: SourceFile,
    bloopPort: Int,
  )(using executionContext: concurrent.ExecutionContextExecutorService): EvaluationContext =
    val currFileParent = new java.io.File(file.path).getAbsoluteFile.toPath.getParent
    val logStream = new java.io.PrintStream(new java.io.FileOutputStream(
      currFileParent.resolve("bloopLog.txt").toFile, true))
    val bloopServer = Bsp4sBloopServerConnection.std(
      currFileParent,
      Logger.default("buildsonnet"),
      scribe.Logger("buildsonnet"),
      bloopPort,
      logStream,
    )
    apply(file, currFileParent, bloopServer)
