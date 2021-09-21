package root

sealed trait EvaluationContext:
  def error(src: Source, message: String): Nothing
  def lookup(src: Source, id: String): EvaluatedJValue
  def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext
  def functionCtx(fn: EvaluatedJValue.JFunction): EvaluationContext
  def bind(id: String, value: JValue): EvaluationContext
  def importFile(fileName: String): EvaluatedJValue
  def importStr(fileName: String): EvaluatedJValue.JString
  def self(src: Source): EvaluatedJValue.JObject
  def `super`(src: Source): EvaluatedJValue.JObject
  def hasSuper: Boolean

sealed trait ObjectEvaluationContext extends EvaluationContext:
  def bind(id: String, value: JValue): ObjectEvaluationContext
  def withSelf(self: EvaluatedJValue.JObject): ObjectEvaluationContext
  def withParent(self: EvaluatedJValue.JObject): ObjectEvaluationContext

sealed trait LazyValue:
  def evaluated: EvaluatedJValue

sealed trait LazyObjectValue extends LazyValue:
  val isHidden: Boolean
  def withHidden(hidden: Boolean): LazyObjectValue

object LazyValue:
  def apply(ctx: EvaluationContext, code: JValue): LazyValue =
    new LazyValue:
      @scala.annotation.threadUnsafe
      lazy val evaluated: EvaluatedJValue = eval(ctx)(code)

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
      lazy val evaluated: EvaluatedJValue = {
        //println("SDFLKJ: " + code.toString)
        eval(ctx)(code)
      }
      def withHidden(hidden: Boolean) = withHiddenImp(hidden, this)

object EvaluationContext:
  private inline def typeString[T <: EvaluatedJValue]: String =
    inline compiletime.erasedValue[T] match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull.type => "null"
    case _: EvaluatedJValue.JString => "string"
    case _: EvaluatedJValue.JNum => "number"
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"

  private def typeString(expr: EvaluatedJValue): String =
    expr match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull.type => "null"
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
    inline def expectArray(code: JValue): EvaluatedJValue.JArray = expectType[EvaluatedJValue.JArray](code)
    inline def expectObject(code: JValue): EvaluatedJValue.JObject = expectType[EvaluatedJValue.JObject](code)
    inline def expectFunction(code: JValue): EvaluatedJValue.JFunction = expectType[EvaluatedJValue.JFunction](code)

    inline def expectType[T <: EvaluatedJValue](code: JValue): T =
      val expr = eval(ctx)(code)
      if expr.isInstanceOf[T] then
        expr.asInstanceOf[T]
      else
        typeError[T](code.src, expr)

  private class Imp(
    val scope: Map[String, LazyValue],
    val stack: List[EvaluatedJValue.JFunction | EvaluatedJValue.JObject],
  ) extends EvaluationContext:
    def error(src: Source, message: String): Nothing =
      throw new Exception(s"$src: $message")

    def lookup(src: Source, id: String): EvaluatedJValue =
      scope.get(id).fold {
        error(src, s"no variable $id defined")
      }(_.evaluated)

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        obj,
        None,
        this,
        Map.empty,
        stack,
      ).bind("$", JValue.JSelf(Source.Generated))

    def functionCtx(fn: EvaluatedJValue.JFunction): Imp =
      new Imp(scope, fn +: stack)

    def bind(id: String, value: JValue): Imp =
      val newScope = scope + (id -> LazyValue(this, value))
      new Imp(newScope, stack)

    def importFile(fileName: String): EvaluatedJValue = ???
    def importStr(fileName: String): EvaluatedJValue.JString = ???

    def self(src: Source): EvaluatedJValue.JObject = error(src, "no self")
    def `super`(src: Source): EvaluatedJValue.JObject = error(src, "no super")
    def hasSuper: Boolean = false

  private case class ObjectImp(
    selfObj: EvaluatedJValue.JObject,
    superOpt: Option[EvaluatedJValue.JObject],
    topCtx: Imp,
    locals: Map[String, JValue],
    stack: List[EvaluatedJValue.JFunction | EvaluatedJValue.JObject],
  ) extends ObjectEvaluationContext:

    def self(src: Source) = selfObj
    def `super`(src: Source) = superOpt.getOrElse(topCtx.`super`(src))
    def hasSuper: Boolean = superOpt.isDefined
    export topCtx.error, topCtx.importStr, topCtx.importFile

    @scala.annotation.threadUnsafe
    lazy val cache = collection.mutable.HashMap[String, EvaluatedJValue]()
    val scope = locals.map { (id, value) =>
      id -> LazyValue(this, value)
    }
    def lookup(src: Source, id: String): EvaluatedJValue =
      if scope.contains(id) then
        scope(id).evaluated
      else
        topCtx.lookup(src, id)

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        obj,
        None,
        new Imp(topCtx.scope ++ scope, topCtx.stack),
        Map.empty,
        stack,
      )

    def functionCtx(fn: EvaluatedJValue.JFunction) =
      this.copy(stack = fn +: stack)

    def withSelf(newSelf: EvaluatedJValue.JObject) =
      if `self` == newSelf then this else this.copy(selfObj = newSelf)

    def withParent(parent: EvaluatedJValue.JObject) =
      if superOpt.isDefined && superOpt.get == parent then
        this
      else
        this.copy(superOpt = Some(parent))

    def bind(id: String, value: JValue) =
      this.copy(locals = locals + (id -> value))

  def apply(): EvaluationContext = Imp(Map.empty, List.empty)

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

sealed trait JObjectImp(
  ctxThunk: () => ObjectEvaluationContext,
  rawMembers: Seq[JObjMember.JField],
):
  private def ctx = ctxThunk()

  private var _cache: collection.mutable.HashMap[String, LazyObjectValue] = null
  lazy val cache = {
    val result = collection.mutable.HashMap[String, LazyObjectValue]()
    rawMembers.foreach { case JObjMember.JField(_, rawKey, plus, isHidden, value) =>
      val key = ctx.expectString(rawKey).str
      if plus then
        result(key) = LazyValue(
          ctx,
          JValue.JBinaryOp(
            Source.Generated,
            JValue.JGetField(Source.Generated, JValue.JSuper(Source.Generated), key),
            JBinaryOperator.Op_+,
            value
          ),
          isHidden
        )
      else
        result(key) = LazyValue(ctx, value, isHidden)
    }
    result
  }

  def lookup(src: Source, field: String): EvaluatedJValue =
    cache.getOrElse(field, ctx.error(src, s"object missing field $field")).evaluated

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
    else
      _members

enum EvaluatedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[EvaluatedJValue])
  /** needs to handle:
    *
    * - simple lookup, defined in current object
    * - super lookup (dynamic), not in current object, but in super object
    * - self lookup (dynamic), lookup in current object, possibly in super
    */
  case JObject(ctx: () => ObjectEvaluationContext, rawMembers: Seq[JObjMember.JField]) extends EvaluatedJValue with JObjectImp(ctx, rawMembers)
  case JFunction(params: JParamList, body: JValue)

  def manifest(ctx: EvaluationContext): Either[String, ManifestedJValue] =
    this match
    case JBoolean(value) => Right(ManifestedJValue.JBoolean(value))
    case JNull => Right(ManifestedJValue.JNull)
    case JString(str) => Right(ManifestedJValue.JString(str))
    case JNum(double) => Right(ManifestedJValue.JNum(double))
    case JArray(elements) =>
      elements.foldLeft(Right(Seq.empty): Either[String, Seq[ManifestedJValue]]) {
        case (Right(tail), element) => element.manifest(ctx).map(_ +: tail)
        case (error, _) => error
      }.map(l => ManifestedJValue.JArray(l.reverse.toVector))
    case obj: JObject =>
      obj.members().toSeq.sortBy(_._1).foldLeft(Right(Seq.empty):Either[String, Seq[(String, ManifestedJValue)]] ) {
        case (Right(tail), (key, value)) => value.evaluated.manifest(ctx).map(v => (key -> v) +: tail)
        case (error, _) => error
      }.map(m => ManifestedJValue.JObject(m.reverse.toMap))
    case fn: JFunction => ctx.error(???, "cannot manifest function")

object EvaluatedJValue:
  extension (arr: EvaluatedJValue.JArray)
    def index(src: Source, ctx: EvaluationContext, idx: Int, endIdxOpt: Option[Int], strideOpt: Option[Int]): EvaluatedJValue =
      println(s"$idx, $endIdxOpt, $strideOpt")
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
          EvaluatedJValue.JArray(Vector.empty)
        else
          val elements = for
            i <- idx until endIdx by stride
            if i < size
          yield arr.elements(i)
          EvaluatedJValue.JArray(elements.toVector)

def eval(ctx: EvaluationContext)(jvalue: JValue): EvaluatedJValue =
  jvalue match
  case _: JValue.JFalse => EvaluatedJValue.JBoolean(false)
  case _: JValue.JTrue => EvaluatedJValue.JBoolean(true)
  case _: JValue.JNull => EvaluatedJValue.JNull
  case JValue.JSelf(src) => ctx.self(src)
  case JValue.JSuper(src) => ctx.`super`(src)
  case JValue.JOuter(src) => ctx.lookup(src, "$")
  case JValue.JString(_, str) => EvaluatedJValue.JString(str)
  case JValue.JNum(_, str) => EvaluatedJValue.JNum(str.toDouble)
  case JValue.JArray(_, elements) => EvaluatedJValue.JArray(elements.map(eval(ctx)))
  case JValue.JObject(_, members) =>
    var objCtx: ObjectEvaluationContext = null
    val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
      () => objCtx,
      members.collect {
        case f: JObjMember.JField => f
      }
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

  case JValue.JId(src, name) => ctx.lookup(src, name)
  case JValue.JGetField(src, loc, field) =>
    ctx
      .expectObject(loc)
      .members()
      .getOrElse(field, ctx.error(src, s"object does not have field $field"))
      .evaluated
  case JValue.JIndex(src, loc, rawIndex) =>
    eval(ctx)(loc) match
    case obj: EvaluatedJValue.JObject =>
      val field = ctx.expectString(rawIndex)
      obj.lookup(src, field.str)
    case EvaluatedJValue.JArray(elements) =>
      val index = ctx.expectNum(rawIndex).double.toInt
      elements(index)
    case expr => ctx.typeError2[EvaluatedJValue.JArray, EvaluatedJValue.JObject](loc.src, expr)
  case JValue.JSlice(src, loc, rawIndex, rawEndIndex, rawStride) =>
    println(s"$rawIndex, $rawEndIndex, $rawStride")
    eval(ctx)(loc) match
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
    val numGivenArgs = positionalArgs.size + namedArgs.size
    if numGivenArgs > fn.params.size then
      ctx.error(src, "to many arguments for function")
    val argMap = namedArgs.toMap
    val (_, argsCtx) = fn.params.foldLeft(positionalArgs -> ctx) {
      case ((positionalArgs, ctx), (argName, default)) =>
        val isGivenNamedArg = argMap.contains(argName)
        if positionalArgs.nonEmpty && isGivenNamedArg then
          ctx.error(src, s"both positional and named arg provided for argument $argName")
        else if positionalArgs.nonEmpty then
          (positionalArgs.tail, ctx.bind(argName, positionalArgs.head))
        else if isGivenNamedArg then
          (positionalArgs, ctx.bind(argName, argMap(argName)))
        else if default.isDefined then
          (positionalArgs, ctx.bind(argName, default.get))
        else
          ctx.error(src, s"missing argument $argName")
    }
    val functionCtx = argsCtx.functionCtx(fn)
    eval(functionCtx)(fn.body)
  case JValue.JBinaryOp(_, left, op, right) =>
    op match
    case JBinaryOperator.Op_+ =>
      (eval(ctx)(left), eval(ctx)(right)) match
      case (op1: EvaluatedJValue.JNum, op2: EvaluatedJValue.JNum) =>
        EvaluatedJValue.JNum(op1.double + op2.double)
      case (op1: EvaluatedJValue.JObject, op2: EvaluatedJValue.JObject) =>
        var parentCtx: ObjectEvaluationContext = null
        val parent: EvaluatedJValue.JObject = op1.copy(ctx = () => parentCtx)
        var resultCtx: ObjectEvaluationContext = null
        val result: EvaluatedJValue.JObject = op2.copy(ctx = () => resultCtx)
        resultCtx = op2.ctx().withSelf(result).withParent(parent)
        parentCtx = op1.ctx().withSelf(result)
        result
      case (op1: EvaluatedJValue.JArray, op2: EvaluatedJValue.JArray) =>
        EvaluatedJValue.JArray(op1.elements ++ op2.elements)
      case (op1: EvaluatedJValue.JNum, op2) => ctx.typeError[EvaluatedJValue.JNum](right.src, op2)
      case (op1, op2: EvaluatedJValue.JNum) => ctx.typeError[EvaluatedJValue.JNum](right.src, op2)
      case (op1: EvaluatedJValue.JObject, op2) => ctx.typeError[EvaluatedJValue.JObject](right.src, op2)
      case (op1, op2: EvaluatedJValue.JObject) => ctx.typeError[EvaluatedJValue.JObject](right.src, op2)
      case (op1, _) =>
        ctx.typeError2[EvaluatedJValue.JNum, EvaluatedJValue.JObject](left.src, op1)

  case JValue.JUnaryOp(_, op, rawOperand) =>
    op match
    case JUnaryOperator.Op_! =>
      val operand = ctx.expectBoolean(rawOperand)
      EvaluatedJValue.JBoolean(!operand.value)
    case JUnaryOperator.Op_+  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(operand.toInt)
    case JUnaryOperator.Op_-  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(-operand.toInt)
    case JUnaryOperator.Op_~  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(~operand.toInt)
  case JValue.JLocal(_, name, value, result) =>
    eval(ctx.bind(name, value))(result)
  case JValue.JFunction(_, params, body) =>
    EvaluatedJValue.JFunction(params, body)
  case JValue.JIf(src, rawCond, trueValue, elseValue) =>
    val cond = ctx.expectBoolean(rawCond)
    if cond.value then
      eval(ctx)(trueValue)
    else
      elseValue.fold(EvaluatedJValue.JNull)(eval(ctx))
  case JValue.JError(src, rawExpr) =>
    val msg = ctx.expectString(rawExpr)
    ctx.error(src, msg.str)
  case JValue.JAssert(src, rawCond, rawMsg, expr) =>
    val cond = ctx.expectBoolean(rawCond)
    if !cond.value then
      val msg = rawMsg.map(msg => ctx.expectString(msg).str)
      ctx.error(src, msg.getOrElse(s"assertion failed"))
    eval(ctx)(expr)
  case JValue.JImport(_, file) =>
    ctx.importFile(file)
  case JValue.JImportStr(_, file) =>
    ctx.importStr(file)
/*
  case JValue.JArrayComprehension(
    forVar: String,
    forExpr: JValue,
    inExpr: JValue,
    cond: Option[JValue]
  )
*/
