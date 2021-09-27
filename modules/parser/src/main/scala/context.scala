package root

sealed trait LazyValue:
  def evaluated: EvaluatedJValue

sealed trait LazyObjectValue extends LazyValue:
  val isHidden: Boolean
  def withHidden(hidden: Boolean): LazyObjectValue

object LazyValue:
  def apply(ctx: EvaluationContext, code: JValue): LazyValue =
    new LazyValue:
      @scala.annotation.threadUnsafe
      lazy val evaluated: EvaluatedJValue = evalUnsafe(ctx)(code)

  def strict(value: EvaluatedJValue): LazyValue =
    new LazyValue:
      val evaluated: EvaluatedJValue = value

  def strictObject(value: EvaluatedJValue, hidden: Boolean): LazyObjectValue =
    new LazyObjectValue:
      val isHidden = hidden
      def withHidden(isHidden: Boolean) = strictObject(value, isHidden)
      val evaluated: EvaluatedJValue = value

  private def withHiddenImp(newIsHidden: Boolean, lazyVal: LazyObjectValue): LazyObjectValue =
    if newIsHidden ^ lazyVal.isHidden then
      new LazyObjectValue:
        val isHidden = newIsHidden
        def evaluated = lazyVal.evaluated
        def withHidden(isHidden: Boolean) = withHiddenImp(isHidden, this)
    else
      lazyVal

  def apply(ctx: EvaluationContext, code: JValue, hidden: Boolean): LazyObjectValue =
    new LazyObjectValue:
      self =>
      val isHidden = hidden
      @scala.annotation.threadUnsafe
      lazy val evaluated: EvaluatedJValue = evalUnsafe(ctx)(code)

      def withHidden(hidden: Boolean) = withHiddenImp(hidden, this)

enum ExprMapper[T <: EvaluatedJValue.JNow]:
  case Future(ctx: concurrent.ExecutionContext, future: concurrent.Future[T])
  case Now(expr: T)

  def map[A <: EvaluatedJValue.JNow](fn: T => A): ExprMapper[A] =
    this match
    case Future(ctx @ given concurrent.ExecutionContext, future) => Future(ctx, future.map(fn))
    case Now(expr) => Now(fn(expr))

final class StackEntry(
  val file: SourceFile,
  val src: Source,
  val message: String,
):
  override def toString: String =
    val srcString = src match
    case Source.Generated => file.path
    case src: Source.Range =>
      val (startLine, startCol) = file.getLineCol(src.start)
      val (endLine, endCol) = file.getLineCol(src.end)
      if startLine == endLine then
        s"${file.path}:$startLine:$startCol-$endCol"
      else
        s"${file.path}:($startLine:$startCol)-($endLine:$endCol)"
    if message.isEmpty then srcString else s"$srcString $message"

object StackEntry:
  def function(file: SourceFile, src: Source): StackEntry =
    new StackEntry(file, src, "function")

  def apply(file: SourceFile, src: Source): StackEntry =
    new StackEntry(file, src, "")

  def objectField(file: SourceFile, src: Source): StackEntry =
    new StackEntry(file, src, "object")

final class EvaluationError(
  file: SourceFile,
  src: Source,
  message: String,
  stack: List[StackEntry],
) extends Exception(EvaluationError.toString(file, src, message, stack))

object EvaluationError:
  def toString(
    file: SourceFile,
    src: Source,
    message: String,
    stack: List[StackEntry],
  ): String =
    val stackSuffix = (StackEntry(file, src) +: stack).mkString("\n  ", "\n  ", "")
    s"$message$stackSuffix"


sealed trait EvaluationContext:
  def executionContext: concurrent.ExecutionContext
  def file: SourceFile
  def error(src: Source, message: String): Nothing
  def lookup(src: Source, id: String): LazyValue
  def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext
  def functionCtx(fn: Source): EvaluationContext
  def bind(id: String, value: JValue): EvaluationContext
  def bindEvaluated(id: String, value: EvaluatedJValue): EvaluationContext
  def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue): EvaluationContext
  def importFile(src: Source, fileName: String): EvaluatedJValue
  def importStr(src: Source, fileName: String): EvaluatedJValue.JString
  def self(src: Source): EvaluatedJValue.JObject
  def `super`(src: Source): EvaluatedJValue.JObject
  def hasSuper: Boolean
  def withStackEntry(entry: StackEntry): EvaluationContext

sealed trait ObjectEvaluationContext extends EvaluationContext:
  def bind(id: String, value: JValue): ObjectEvaluationContext
  def bindEvaluated(id: String, value: EvaluatedJValue): ObjectEvaluationContext
  def withSelf(self: EvaluatedJValue.JObject): ObjectEvaluationContext
  def withParent(self: EvaluatedJValue.JObject): ObjectEvaluationContext
  def withStackEntry(entry: StackEntry): ObjectEvaluationContext

object EvaluationContext:
  inline private def typeString[T]: String =
    val types = macros.mapUnionType[T, String] {
      case _: EvaluatedJValue.JBoolean => "bool"
      case _: EvaluatedJValue.JNull => "null"
      case _: EvaluatedJValue.JString => "string"
      case _: EvaluatedJValue.JNum => "number"
      case _: EvaluatedJValue.JArray => "array"
      case _: EvaluatedJValue.JObject => "object"
      case _: EvaluatedJValue.JFunction => "function"
      case _: EvaluatedJValue.JFuture => "future"
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
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"
    case _: EvaluatedJValue.JFuture => "future"

  import concurrent.Future
  extension (ctx: EvaluationContext)
    inline def expectBoolean(code: JValue): Future[EvaluatedJValue.JBoolean] = expectType[EvaluatedJValue.JBoolean](code)
    inline def expectBoolean(expr: EvaluatedJValue): Future[EvaluatedJValue.JBoolean] = expectType[EvaluatedJValue.JBoolean](expr)
    inline def expectNum(code: JValue): Future[EvaluatedJValue.JNum] = expectType[EvaluatedJValue.JNum](code)
    inline def expectNum(expr: EvaluatedJValue): Future[EvaluatedJValue.JNum] = expectType[EvaluatedJValue.JNum](expr)
    inline def expectString(code: JValue): Future[EvaluatedJValue.JString] = expectType[EvaluatedJValue.JString](code)
    inline def expectString(expr: EvaluatedJValue): Future[EvaluatedJValue.JString] = expectType[EvaluatedJValue.JString](expr)
    inline def expectFieldName(code: JValue): Future[EvaluatedJValue.JString | EvaluatedJValue.JNull] =
      val expr = evalUnsafe(ctx)(code)
      expectType[EvaluatedJValue.JString | EvaluatedJValue.JNull](expr, s"Field name must be string or null, got ${typeString(expr)}")
    inline def expectArray(code: JValue): Future[EvaluatedJValue.JArray] = expectType[EvaluatedJValue.JArray](code)
    inline def expectArray(expr: EvaluatedJValue): Future[EvaluatedJValue.JArray] = expectType[EvaluatedJValue.JArray](expr)
    inline def expectObject(code: JValue): Future[EvaluatedJValue.JObject] = expectType[EvaluatedJValue.JObject](code)
    inline def expectObject(expr: EvaluatedJValue): Future[EvaluatedJValue.JObject] = expectType[EvaluatedJValue.JObject](expr)
    inline def expectFunction(code: JValue): Future[EvaluatedJValue.JFunction] = expectType[EvaluatedJValue.JFunction](code)
    inline def expectFunction(expr: EvaluatedJValue): Future[EvaluatedJValue.JFunction] = expectType[EvaluatedJValue.JFunction](expr)

    inline def expectType[T <: EvaluatedJValue.JNow](expr: EvaluatedJValue, msg: String): Future[T] =
      implicit val ec = ctx.executionContext
      expr match
      case t: T => Future(t)
      case f: EvaluatedJValue.JFuture => f.future.map {
        case t: T => t
        case _ => ctx.error(expr.src, msg)
      }
      case _ => ctx.error(expr.src, msg)

    inline def expectType[T <: EvaluatedJValue.JNow](expr: EvaluatedJValue): Future[T] =
      expectType[T](expr, s"Unexpected type ${typeString(expr)}, expected ${typeString[T]}")

    // inline def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue): T =
    //   if expr.isInstanceOf[T] then
    //     expr.asInstanceOf[T]
    //   else
    //     ctx.error(expr.src, s"Unexpected type ${typeString(expr)}, expected ${typeString[T]}")

    inline def expectType[T <: EvaluatedJValue.JNow](code: JValue): Future[T] =
      expectType[T](evalUnsafe(ctx)(code))

  private[root] case class Imp(
    val file: SourceFile,
    val scope: Map[String, LazyValue],
    val stack: List[StackEntry],
    val executionContext: concurrent.ExecutionContext,
  ) extends EvaluationContext:
    def error(src: Source, message: String): Nothing = throw new EvaluationError(file, src, message, stack)

    def lookup(src: Source, id: String): LazyValue =
      scope.get(id).getOrElse(
        error(src, s"no variable $id defined")
      )

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        obj,
        None,
        this,
        Map.empty,
        stack,
      ).bind("$", JValue.JSelf(Source.Generated))

    def functionCtx(fn: Source) =
      this.copy(stack = StackEntry.function(file, fn) +: stack)

    def bind(id: String, value: JValue) =
      val newScope = scope + (id -> LazyValue(this, value))
      this.copy(scope = newScope)

    def bindEvaluated(id: String, value: EvaluatedJValue) =
      val newScope = scope + (id -> LazyValue.strict(value))
      this.copy(scope = newScope)

    def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue) =
      val newScope = scope + (id -> LazyValue(ctx, value))
      this.copy(scope = newScope)

    def importFile(src: Source, fileName: String): EvaluatedJValue = ???
    def importStr(src: Source, fileName: String): EvaluatedJValue.JString = ???

    def self(src: Source): EvaluatedJValue.JObject = error(src, "no self")
    def `super`(src: Source): EvaluatedJValue.JObject = error(src, "no super")
    def hasSuper: Boolean = false
    def withStackEntry(entry: StackEntry) = this.copy(stack = entry +: stack)

  private[root] case class ObjectImp(
    selfObj: EvaluatedJValue.JObject,
    superOpt: Option[EvaluatedJValue.JObject],
    topCtx: Imp,
    locals: Map[String, JValue | EvaluatedJValue | LazyValue],
    stack: List[StackEntry],
  ) extends ObjectEvaluationContext:
    def executionContext = topCtx.executionContext

    def file = topCtx.file
    def self(src: Source) = selfObj
    def `super`(src: Source) = superOpt.getOrElse(topCtx.`super`(src))
    def hasSuper: Boolean = superOpt.isDefined

    def error(src: Source, message: String): Nothing = throw new EvaluationError(file, src, message, stack)
    export topCtx.importStr, topCtx.importFile

    @scala.annotation.threadUnsafe
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

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        obj,
        None,
        topCtx.copy(scope = topCtx.scope ++ scope),
        Map.empty,
        stack,
      )

    def functionCtx(fn: Source) =
      this.copy(stack = StackEntry.function(file, fn) +: stack)

    def withSelf(newSelf: EvaluatedJValue.JObject) =
      if `self` == newSelf then this else this.copy(selfObj = newSelf)

    def withParent(parent: EvaluatedJValue.JObject) =
      if superOpt.isDefined && superOpt.get == parent then
        this
      else
        this.copy(superOpt = Some(parent))

    def bind(id: String, value: JValue) =
      this.copy(locals = locals + (id -> value))

    def bindEvaluated(id: String, value: EvaluatedJValue) =
      this.copy(locals = locals + (id -> value))

    def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue) =
      this.copy(locals = locals + (id -> LazyValue(ctx, value)))

    def withStackEntry(entry: StackEntry) =
      this.copy(stack = entry +: stack)

  def apply(file: SourceFile)(implicit executionContext: concurrent.ExecutionContext): EvaluationContext =
    Imp(file, Map.empty, List.empty, executionContext)
