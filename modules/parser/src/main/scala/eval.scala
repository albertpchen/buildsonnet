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
  private inline def typeString[T <: EvaluatedJValue]: String =
    inline compiletime.erasedValue[T] match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull => "null"
    case _: EvaluatedJValue.JString => "string"
    case _: EvaluatedJValue.JNum => "number"
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"

  private def typeString(expr: EvaluatedJValue): String =
    expr match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull => "null"
    case _: EvaluatedJValue.JString => "string"
    case _: EvaluatedJValue.JNum => "number"
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"

  extension (ctx: EvaluationContext)
    inline def typeError2[T1 <: EvaluatedJValue, T2 <: EvaluatedJValue](src: Source, expr: EvaluatedJValue): T1 | T2 =
      ctx.error(src, s"Unexpected type ${typeString(expr)}, expected ${typeString[T1]} or ${typeString[T2]}")

    inline def typeError[T <: EvaluatedJValue](src: Source, expr: EvaluatedJValue): T =
      ctx.error(src, s"Unexpected type ${typeString(expr)}, expected ${typeString[T]}")

    inline def expectBoolean(code: JValue): EvaluatedJValue.JBoolean = expectType[EvaluatedJValue.JBoolean](code)
    inline def expectNum(code: JValue): EvaluatedJValue.JNum = expectType[EvaluatedJValue.JNum](code)
    inline def expectString(code: JValue): EvaluatedJValue.JString = expectType[EvaluatedJValue.JString](code)
    inline def expectFieldName(code: JValue): EvaluatedJValue.JString =
      val expr = evalUnsafe(ctx)(code)
      if expr.isInstanceOf[EvaluatedJValue.JString] then
        expr.asInstanceOf[EvaluatedJValue.JString]
      else
        ctx.error(code.src, s"Field name must be string, got ${typeString(expr)}")
    inline def expectArray(code: JValue): EvaluatedJValue.JArray = expectType[EvaluatedJValue.JArray](code)
    inline def expectObject(code: JValue): EvaluatedJValue.JObject = expectType[EvaluatedJValue.JObject](code)
    inline def expectFunction(code: JValue): EvaluatedJValue.JFunction = expectType[EvaluatedJValue.JFunction](code)

    inline def expectType[T <: EvaluatedJValue](code: JValue): T =
      val expr = evalUnsafe(ctx)(code)
      if expr.isInstanceOf[T] then
        expr.asInstanceOf[T]
      else
        typeError[T](code.src, expr)

  private class Imp(
    val file: SourceFile,
    val scope: Map[String, LazyValue],
    val stack: List[StackEntry],
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
      new Imp(file, scope, StackEntry.function(file, fn) +: stack)

    def bind(id: String, value: JValue) =
      val newScope = scope + (id -> LazyValue(this, value))
      new Imp(file, newScope, stack)

    def bindEvaluated(id: String, value: EvaluatedJValue) =
      val newScope = scope + (id -> LazyValue.strict(value))
      new Imp(file, newScope, stack)

    def bindWithCtx(id: String, ctx: EvaluationContext, value: JValue) =
      val newScope = scope + (id -> LazyValue(ctx, value))
      new Imp(file, newScope, stack)

    def importFile(src: Source, fileName: String): EvaluatedJValue = ???
    def importStr(src: Source, fileName: String): EvaluatedJValue.JString = ???

    def self(src: Source): EvaluatedJValue.JObject = error(src, "no self")
    def `super`(src: Source): EvaluatedJValue.JObject = error(src, "no super")
    def hasSuper: Boolean = false
    def withStackEntry(entry: StackEntry) = new Imp(file, scope, entry +: stack)

  private case class ObjectImp(
    selfObj: EvaluatedJValue.JObject,
    superOpt: Option[EvaluatedJValue.JObject],
    topCtx: Imp,
    locals: Map[String, JValue | EvaluatedJValue | LazyValue],
    stack: List[StackEntry],
  ) extends ObjectEvaluationContext:

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
        new Imp(topCtx.file, topCtx.scope ++ scope, topCtx.stack),
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

  def apply(file: SourceFile): EvaluationContext = Imp(file, Map.empty, List.empty)

case class ManifestError(msg: String)

enum ManifestedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Vector[ManifestedJValue])
  case JObject(members: Map[String, ManifestedJValue])

  override def toString: String =
    val builder = new StringBuilder
    prettyPrintImp("  ", 0, None, builder)
    builder.toString

  private def prettyPrintImp(
    tab: String,
    tabNum: Int,
    firstPrefix: Option[String],
    builder: StringBuilder
  ): Unit =
    val prefix = tab * tabNum
    builder ++= firstPrefix.getOrElse(prefix)
    this match
      case JNull => builder ++= "null"
      case JString(value) =>
        builder += '"'
        ManifestedJValue.escape(value, builder)
        builder += '"'
      case JNum(value) =>
        if value.isWhole then
          builder ++= value.toLong.toString
        else
          builder ++= value.toString
      case JBoolean(value) => builder ++= value.toString
      case JArray(value) if value.isEmpty => builder ++= "[]"
      case JArray(value) =>
        builder ++= "[\n"
        value.head.prettyPrintImp(tab, tabNum + 1, None, builder)
        value.tail.foreach { e =>
          builder ++= ",\n"
          e.prettyPrintImp(tab, tabNum + 1, None, builder)
        }
        builder += '\n'
        builder ++= prefix
        builder += ']'
      case JObject(value) if value.isEmpty => builder ++= "{}"
      case JObject(unsorted) =>
        val value = unsorted.toSeq.sortBy(_._1)
        builder ++= "{\n"
        builder ++= prefix
        builder ++= tab
        builder += '"'
        ManifestedJValue.escape(value.head._1, builder)
        value.head._2.prettyPrintImp(tab, tabNum + 1, Some("\": "), builder)
        value.tail.foreach { case (k, v) =>
          builder ++= ",\n"
          builder ++= prefix
          builder ++= tab
          builder += '"'
          ManifestedJValue.escape(k, builder)
          v.prettyPrintImp(tab, tabNum + 1, Some("\": "), builder)
        }
        builder += '\n'
        builder ++= prefix
        builder += '}'

object ManifestedJValue:
  private def escape(s: String, builder: StringBuilder): Unit =
    var idx = 0
    val len = s.length
    while idx < len do
      (s(idx): @annotation.switch) match
        case '"'  => builder ++= "\\\""
        case '\\' => builder ++= "\\\\"
        case '\b' => builder ++= "\\b"
        case '\f' => builder ++= "\\f"
        case '\n' => builder ++= "\\n"
        case '\r' => builder ++= "\\r"
        case '\t' => builder ++= "\\t"
        case c =>
          val shouldEscape = (c >= '\u0000' && c <= '\u001f')
          || (c >= '\u0080' && c < '\u00a0')
          || (c >= '\u2000' && c < '\u2100')
          if shouldEscape then
            builder ++= "\\u%04x".format(c: Int)
          else
            builder ++= c.toString
      idx += 1

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

sealed trait EvaluatedJObject:
  def ctx: ObjectEvaluationContext
  protected def cache: collection.Map[String, LazyObjectValue]

  def withCtx(newCtx: () => ObjectEvaluationContext): EvaluatedJObject

  def lookup(src: Source, field: String): LazyValue =
    cache.getOrElse(field, ctx.error(src, s"object missing field $field"))

  private var _members: collection.Map[String, LazyObjectValue] = null
  def members(): collection.Map[String, LazyObjectValue] =
    if _members == null then
      _members =
        if ctx.hasSuper then
          val superMembers = ctx.`super`(Source.Generated).members()
          superMembers ++ cache.map { (k, v) =>
            val newValue =
              if superMembers.contains(k) && superMembers(k).isHidden && !v.isHidden then
                v.withHidden(superMembers(k).isHidden)
              else
                v
            k -> newValue
          }
        else
          cache
    _members

object EvaluatedJObject:
  def comprehension(
    ctxThunk: () => ObjectEvaluationContext,
    arrayElements: Seq[(String, EvaluatedJValue)],
    forVar: String,
    value: JValue,
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()
    def withCtx(newCtx: () => ObjectEvaluationContext) =
      comprehension(newCtx, arrayElements, forVar, value)

    lazy val cache = {
      val result = collection.mutable.HashMap[String, LazyObjectValue]()
      arrayElements.foreach { (key, e) =>
        val valueCtx = ctx.bindEvaluated(forVar, e).withStackEntry(StackEntry.objectField(ctx.file, value.src))
        result(key) = LazyValue(valueCtx, value, false)
      }
      result
    }

  def apply(
    ctxThunk: () => ObjectEvaluationContext,
    rawMembers: Seq[JObjMember.JField],
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()

    def withCtx(newCtx: () => ObjectEvaluationContext) =
      apply(newCtx, rawMembers)

    lazy val cache = {
      val result = collection.mutable.HashMap[String, LazyObjectValue]()
      rawMembers.foreach { case JObjMember.JField(src, rawKey, plus, isHidden, value) =>
        val key = ctx.expectFieldName(rawKey).str
        val valueCtx = ctx.withStackEntry(StackEntry.objectField(ctx.file, value.src))
        if plus then
          result(key) = LazyValue(
            valueCtx,
            JValue.JBinaryOp(
              src,
              JValue.JGetField(src, JValue.JSuper(src), key),
              JBinaryOperator.Op_+,
              value
            ),
            isHidden
          )
        else
          result(key) = LazyValue(valueCtx, value, isHidden)
      }
      result
    }

final class EvaluatedJFunctionParameters(
  val src: Source,
  val positionalArgs: Seq[JValue],
  val namedArgs: Seq[(String, JValue)],
)

enum EvaluatedJValue extends HasSource:
  case JBoolean(src: Source, value: Boolean)
  case JNull(src: Source)
  case JString(src: Source, str: String)
  case JNum(src: Source, double: Double)
  case JArray(src: Source, elements: Seq[EvaluatedJValue])
  /** needs to handle:
    *
    * - simple lookup, defined in current object
    * - super lookup (dynamic), not in current object, but in super object
    * - self lookup (dynamic), lookup in current object, possibly in super
    */
  case JObject(src: Source, imp: EvaluatedJObject) extends EvaluatedJValue
  case JFunction(src: Source, params: JParamList, body: JValue, fn: (EvaluationContext, EvaluatedJFunctionParameters) => EvaluatedJValue)

  def manifest(ctx: EvaluationContext): Either[String, ManifestedJValue] =
    this match
    case value: JBoolean => Right(ManifestedJValue.JBoolean(value.value))
    case value: JNull => Right(ManifestedJValue.JNull)
    case value: JString => Right(ManifestedJValue.JString(value.str))
    case value: JNum => Right(ManifestedJValue.JNum(value.double))
    case value: JArray =>
      value.elements.foldLeft(Right(Seq.empty): Either[String, Seq[ManifestedJValue]]) {
        case (Right(tail), element) => element.manifest(ctx).map(_ +: tail)
        case (error, _) => error
      }.map(l => ManifestedJValue.JArray(l.reverse.toVector))
    case obj: JObject =>
      val members = obj.members()
      members.toSeq.sortBy(_._1).foldLeft(Right(Seq.empty):Either[String, Seq[(String, ManifestedJValue)]]) {
        case (Right(tail), (key, value)) => value.evaluated.manifest(ctx).map(v => (key -> v) +: tail)
        case (error, _) => error
      }.map(m => ManifestedJValue.JObject(m.reverse.toMap))
    case fn: JFunction => ctx.error(fn.src, "cannot manifest function")

object EvaluatedJValue:
  extension (obj: EvaluatedJValue.JObject)
    // export obj.imp.lookup, obj.imp.members, obj.imp.withCtx
    def ctx: ObjectEvaluationContext = obj.imp.ctx

    def withCtx(ctx: () => ObjectEvaluationContext): EvaluatedJValue.JObject =
      obj.copy(imp = obj.imp.withCtx(ctx))

    def lookup(src: Source, field: String): EvaluatedJValue =
      obj.imp.lookup(src, field).evaluated

    def members(): collection.Map[String, LazyObjectValue] =
      obj.imp.members()

  extension (arr: EvaluatedJValue.JArray)
    def index(src: Source, ctx: EvaluationContext, idx: Int, endIdxOpt: Option[Int], strideOpt: Option[Int]): EvaluatedJValue =
      // println(s"$idx, $endIdxOpt, $strideOpt")
      if idx < 0 || endIdxOpt.exists(_ < 0) || strideOpt.exists(_ < 0) then
        ctx.error(src, s"negative index, end, or stride are not allowed")
      val size = arr.elements.size
      if size <= idx then ctx.error(src, s"index out of bounds $idx")
      if endIdxOpt.isEmpty && strideOpt.isEmpty then
        arr.elements(idx)
      else
        val stride = strideOpt.getOrElse(1)
        val endIdx = endIdxOpt.getOrElse(size - 1)
        if idx >= endIdx then
          EvaluatedJValue.JArray(src, Vector.empty)
        else
          val elements = for
            i <- idx until endIdx by stride
            if i < size
          yield arr.elements(i)
          EvaluatedJValue.JArray(src, elements.toVector)

def manifest(ctx: EvaluationContext)(jvalue: JValue): Either[EvaluationError, ManifestedJValue] =
  try
    val evaluated = evalUnsafe(ctx)(jvalue)
    val result = evaluated.manifest(ctx).fold(
      msg => Left(new EvaluationError(ctx.file, Source.Generated, msg, List.empty)),
      Right(_),
    )
    result
  catch
    case err: EvaluationError => Left(err)

private def applyArgs(
  defCtx: EvaluationContext,
  fnSrc: Source,
  paramsDef: JParamList,
  body: JValue,
)(applyCtx: EvaluationContext, params: EvaluatedJFunctionParameters): EvaluatedJValue =
  val positionalArgs = params.positionalArgs
  val namedArgs = params.namedArgs
  val numGivenArgs = positionalArgs.size + namedArgs.size
  if numGivenArgs > paramsDef.size then
    applyCtx.error(params.src, "to many arguments for function")
  val argMap = namedArgs.toMap
  val (_, argsCtx) = paramsDef.foldLeft(positionalArgs -> defCtx) {
    case ((positionalArgs, ctx), (argName, default)) =>
      val isGivenNamedArg = argMap.contains(argName)
      if positionalArgs.nonEmpty && isGivenNamedArg then
        applyCtx.error(params.src, s"both positional and named arg provided for argument $argName")
      else if positionalArgs.nonEmpty then
        (positionalArgs.tail, ctx.bindWithCtx(argName, applyCtx, positionalArgs.head))
      else if isGivenNamedArg then
        (positionalArgs, ctx.bindWithCtx(argName, applyCtx, argMap(argName)))
      else if default.isDefined then
        (positionalArgs, ctx.bindWithCtx(argName, applyCtx, default.get))
      else
        applyCtx.error(params.src, s"missing argument $argName")
  }
  val functionCtx = argsCtx.functionCtx(fnSrc)
  evalUnsafe(functionCtx)(body)

def evalUnsafe(ctx: EvaluationContext)(jvalue: JValue): EvaluatedJValue =
  jvalue match
  case JValue.JFalse(src) => EvaluatedJValue.JBoolean(src, false)
  case JValue.JTrue(src) => EvaluatedJValue.JBoolean(src, true)
  case JValue.JNull(src) => EvaluatedJValue.JNull(src)
  case JValue.JSelf(src) => ctx.self(src)
  case JValue.JSuper(src) => ctx.`super`(src)
  case JValue.JOuter(src) => ctx.lookup(src, "$").evaluated
  case JValue.JString(src, str) => EvaluatedJValue.JString(src, str)
  case JValue.JNum(src, str) => EvaluatedJValue.JNum(src, str.toDouble)
  case JValue.JArray(src, elements) => EvaluatedJValue.JArray(src, elements.map(evalUnsafe(ctx)))
  case JValue.JObject(src, members) =>
    var objCtx: ObjectEvaluationContext = null
    val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
      src,
      EvaluatedJObject(
        () => objCtx,
        members.collect {
          case f: JObjMember.JField => f
        }
      ),
    )
    objCtx = members.foldLeft(ctx.objectCtx(obj)) {
      case (ctx, local: JObjMember.JLocal) => ctx.bind(local.name, local.value)
      case (ctx, _) => ctx
    }

    // evaluate asserts inside object
    members.foreach {
      case JObjMember.JAssert(src, rawCond, rawMsg) =>
        val cond = objCtx.expectBoolean(rawCond).value
        val msgOpt = rawMsg.map(msg => objCtx.expectString(msg).str)
        if !cond then objCtx.error(src, msgOpt.getOrElse("object assertion failed"))
      case _ =>
    }
    obj

  case JValue.JObjectComprehension(src, preLocals, rawKey, value, postLocals, forVar, inExpr, condOpt) =>
    var objCtx: ObjectEvaluationContext = null
    val arr = ctx.expectArray(inExpr).elements
    val arrElements =
      if condOpt.isDefined then
        val cond = condOpt.get
        arr.flatMap { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          val key = forCtx.expectFieldName(rawKey).str
          Option.when(ctx.expectBoolean(cond).value) {
            key -> evalUnsafe(forCtx)(inExpr)
          }
        }
      else
        arr.map { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          val key = forCtx.expectFieldName(rawKey).str
          key -> evalUnsafe(forCtx)(inExpr)
        }
    val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
      src,
      EvaluatedJObject.comprehension(
        () => objCtx,
        arrElements,
        forVar,
        value,
      )
    )
    objCtx = postLocals.foldLeft(preLocals.foldLeft(ctx.objectCtx(obj)) { (ctx, local) =>
      ctx.bind(local.name, local.value)
    }) { (ctx, local) =>
      ctx.bind(local.name, local.value)
    }

    obj
  case JValue.JId(src, name) => ctx.lookup(src, name).evaluated
  case JValue.JGetField(src, loc, field) =>
    ctx
      .expectObject(loc)
      .members()
      .getOrElse(field, ctx.error(src, s"object does not have field $field"))
      .evaluated
  case JValue.JIndex(src, loc, rawIndex) =>
    evalUnsafe(ctx)(loc) match
    case obj: EvaluatedJValue.JObject =>
      val field = ctx.expectString(rawIndex)
      obj.lookup(src, field.str)
    case arr: EvaluatedJValue.JArray =>
      val index = ctx.expectNum(rawIndex).double.toInt
      arr.elements(index)
    case expr => ctx.typeError2[EvaluatedJValue.JArray, EvaluatedJValue.JObject](loc.src, expr)
  case JValue.JSlice(src, loc, rawIndex, rawEndIndex, rawStride) =>
    // println(s"$rawIndex, $rawEndIndex, $rawStride")
    evalUnsafe(ctx)(loc) match
    case obj: EvaluatedJValue.JObject =>
      ctx.error(src, "no end index or stride allowed for object index")
    case arr: EvaluatedJValue.JArray =>
      val index = ctx.expectNum(rawIndex).double.toInt
      val endIndex = rawEndIndex.map(ctx.expectNum(_).double.toInt)
      val stride = rawStride.map(ctx.expectNum(_).double.toInt)
      arr.index(src, ctx, index, endIndex, stride)
    case expr => ctx.typeError2[EvaluatedJValue.JArray, EvaluatedJValue.JObject](loc.src, expr)
  case JValue.JApply(src, loc, positionalArgs, namedArgs) =>
    val fn = ctx.expectFunction(loc)
    val params = EvaluatedJFunctionParameters(src, positionalArgs, namedArgs)
    fn.fn(ctx, params)
  case JValue.JBinaryOp(src, left, op, right) =>
    op match
    case JBinaryOperator.Op_+ =>
      (evalUnsafe(ctx)(left), evalUnsafe(ctx)(right)) match
      case (op1: EvaluatedJValue.JNum, op2: EvaluatedJValue.JNum) =>
        EvaluatedJValue.JNum(src, op1.double + op2.double)
      case (op1: EvaluatedJValue.JObject, op2: EvaluatedJValue.JObject) =>
        var parentCtx: ObjectEvaluationContext = null
        val parent: EvaluatedJValue.JObject = op1.withCtx(ctx = () => parentCtx)
        var resultCtx: ObjectEvaluationContext = null
        val result: EvaluatedJValue.JObject = op2.withCtx(ctx = () => resultCtx)
        resultCtx = op2.ctx.withSelf(result).withParent(parent)
        parentCtx = op1.ctx.withSelf(result)
        result
      case (op1: EvaluatedJValue.JArray, op2: EvaluatedJValue.JArray) =>
        EvaluatedJValue.JArray(src, op1.elements ++ op2.elements)
      case (op1: EvaluatedJValue.JNum, op2) => ctx.typeError[EvaluatedJValue.JNum](right.src, op2)
      case (op1, op2: EvaluatedJValue.JNum) => ctx.typeError[EvaluatedJValue.JNum](right.src, op2)
      case (op1: EvaluatedJValue.JObject, op2) => ctx.typeError[EvaluatedJValue.JObject](right.src, op2)
      case (op1, op2: EvaluatedJValue.JObject) => ctx.typeError[EvaluatedJValue.JObject](right.src, op2)
      case (op1, _) =>
        ctx.typeError2[EvaluatedJValue.JNum, EvaluatedJValue.JObject](left.src, op1)

  case JValue.JUnaryOp(src, op, rawOperand) =>
    op match
    case JUnaryOperator.Op_! =>
      val operand = ctx.expectBoolean(rawOperand)
      EvaluatedJValue.JBoolean(src, !operand.value)
    case JUnaryOperator.Op_+  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(src, operand)
    case JUnaryOperator.Op_-  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(src, -operand)
    case JUnaryOperator.Op_~  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(src, ~operand.toLong)
  case JValue.JLocal(_, name, value, result) =>
    evalUnsafe(ctx.bind(name, value))(result)
  case JValue.JFunction(src, params, body) =>
    EvaluatedJValue.JFunction(src, params, body, applyArgs(ctx, src, params, body))
  case JValue.JIf(src, rawCond, trueValue, elseValue) =>
    val cond = ctx.expectBoolean(rawCond)
    if cond.value then
      evalUnsafe(ctx)(trueValue)
    else
      elseValue.fold(EvaluatedJValue.JNull(src))(evalUnsafe(ctx))
  case JValue.JError(src, rawExpr) =>
    val msg = ctx.expectString(rawExpr)
    ctx.error(src, msg.str)
  case JValue.JAssert(src, rawCond, rawMsg, expr) =>
    val cond = ctx.expectBoolean(rawCond)
    if !cond.value then
      val msg = rawMsg.map(msg => ctx.expectString(msg).str)
      ctx.error(src, msg.getOrElse(s"assertion failed"))
    evalUnsafe(ctx)(expr)
  case JValue.JImport(src, file) =>
    ctx.importFile(src, file)
  case JValue.JImportStr(src, file) =>
    ctx.importStr(src, file)
  case JValue.JArrayComprehension(src, forVar, forExpr, inExpr, condOpt) =>
    if condOpt.isDefined then
      val cond = condOpt.get
      EvaluatedJValue.JArray(src, ctx.expectArray(inExpr).elements.flatMap { e =>
        val forCtx = ctx.bindEvaluated(forVar, e)
        Option.when(forCtx.expectBoolean(cond).value) {
          evalUnsafe(forCtx)(forExpr)
        }
      })
    else
      EvaluatedJValue.JArray(src, ctx.expectArray(inExpr).elements.map { e =>
        evalUnsafe(ctx.bindEvaluated(forVar, e))(forExpr)
      })
