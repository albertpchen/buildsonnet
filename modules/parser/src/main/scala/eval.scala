package root

sealed trait EvaluationContext:
  def error(message: String): Nothing
  def lookup(id: String): EvaluatedJValue
  def makeObject(members: Seq[JObjMember]): EvaluatedJValue.JObject
  def functionCtx(fn: EvaluatedJValue.JFunction): EvaluationContext
  def bind(id: String, value: JValue): EvaluationContext
  def importFile(fileName: String): EvaluatedJValue
  def importStr(fileName: String): EvaluatedJValue.JString
  def self(): EvaluatedJValue.JObject
  def outer(): EvaluatedJValue.JObject

sealed trait LazyValue:
  lazy val evaluated: EvaluatedJValue

sealed trait LazyObjectValue extends LazyValue:
  val isHidden: Boolean

object LazyValue:
  def apply(ctx: EvaluationContext, code: JValue): LazyValue =
    new LazyValue:
      @scala.annotation.threadUnsafe
      lazy val evaluated: EvaluatedJValue = eval(ctx)(code)

  def apply(ctx: EvaluationContext, code: JValue, hidden: Boolean): LazyObjectValue =
    new LazyObjectValue:
      val isHidden = hidden
      @scala.annotation.threadUnsafe
      lazy val evaluated: EvaluatedJValue = {
        eval(ctx)(code)
      }

object EvaluationContext:

  private inline def typeString[T <: EvaluatedJValue]: String =
    inline compiletime.erasedValue[T] match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull.type => "null"
    case _: EvaluatedJValue.JString => "str"
    case _: EvaluatedJValue.JNum => "num"
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"

  private def typeString(expr: EvaluatedJValue): String =
    expr match
    case _: EvaluatedJValue.JBoolean => "bool"
    case _: EvaluatedJValue.JNull.type => "null"
    case _: EvaluatedJValue.JString => "str"
    case _: EvaluatedJValue.JNum => "num"
    case _: EvaluatedJValue.JArray => "array"
    case _: EvaluatedJValue.JObject => "object"
    case _: EvaluatedJValue.JFunction => "function"

  extension (ctx: EvaluationContext)
    inline def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue): T =
      if expr.isInstanceOf[T] then
        expr.asInstanceOf[T]
      else
        ctx.error(s"got type ${typeString(expr)}, expected ${typeString[T]}")

  private case class Imp(
    val selfOpt: Option[EvaluatedJValue.JObject],
    val outerOpt: Option[EvaluatedJValue.JObject],
    val scope: Map[String, LazyValue],
    val stack: List[EvaluatedJValue.JFunction | EvaluatedJValue.JObject],
  ) extends EvaluationContext:
    def self() = selfOpt.getOrElse(error("self may only be called within an object"))
    def outer() = outerOpt.getOrElse(error("$ may only be called within an object"))
    def error(message: String): Nothing = throw new Exception(message)

    def lookup(id: String): EvaluatedJValue =
      scope.get(id).fold {
        error(s"no variable $id defined")
      }(_.evaluated)

    def makeObject(members: Seq[JObjMember]): EvaluatedJValue.JObject =
      var objInsideOpt: Option[Map[String, LazyObjectValue]] = None
      val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(() => objInsideOpt.get)

      val objCtx = {
        val withSelf = this.copy(selfOpt = Some(obj), outerOpt = Some(outerOpt.getOrElse(obj)))
        val result = members.foldLeft(withSelf) {
          case (ctx, local: JObjMember.JLocal) => ctx.bind(local.name, local.value)
          case (ctx, _) => ctx
        }
        result
      }

      objInsideOpt = Some {
        val result = members.collect {
          case JObjMember.JField(rawKey, isHidden, value) =>
            val key = this.expectType[EvaluatedJValue.JString](eval(this)(rawKey)).str
            key -> LazyValue(objCtx, value, isHidden)
        }.toMap
        result
      }

      members.foreach {
        case JObjMember.JAssert(rawCond, rawMsg) =>
          val cond = objCtx.expectType[EvaluatedJValue.JBoolean](eval(objCtx)(rawCond)).value
          val msgOpt = rawMsg.map(msg => objCtx.expectType[EvaluatedJValue.JString](eval(objCtx)(msg)).str)
          if !cond then objCtx.error(msgOpt.getOrElse("object assertion failed"))
        case _ =>
      }
      obj

    def functionCtx(fn: EvaluatedJValue.JFunction): Imp =
      this.copy(stack = fn +: stack)

    def bind(id: String, value: JValue): Imp =
      val newScope = scope + (id -> LazyValue(this, value))
      this.copy(scope = newScope)

    def importFile(fileName: String): EvaluatedJValue = ???
    def importStr(fileName: String): EvaluatedJValue.JString = ???

  def apply(): EvaluationContext = Imp(None, None, Map.empty, List.empty)

case class ManifestError(msg: String)

enum ManifestedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[ManifestedJValue])
  case JObject(members: Map[String, ManifestedJValue])

enum EvaluatedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[EvaluatedJValue])
  case JObject(members: () => Map[String, LazyObjectValue])
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
      }.map(l => ManifestedJValue.JArray(l.reverse))
    case JObject(members) =>
      members().toSeq.sortBy(_._1).foldLeft(Right(Seq.empty):Either[String, Seq[(String, ManifestedJValue)]] ) {
        case (Right(tail), (key, value)) => value.evaluated.manifest(ctx).map(v => (key -> v) +: tail)
        case (error, _) => error
      }.map(m => ManifestedJValue.JObject(m.reverse.toMap))
    case _: JFunction => ctx.error("cannot manifest function")


object EvaluatedJValue:
  extension (obj: JObject)
    def lookup(field: String): EvaluatedJValue = ???

  extension (arr: JArray)
    def index(idx: Int, endIdx: Option[Int], stride: Option[Int]): EvaluatedJValue = ???


object eval:

  def apply(ctx: EvaluationContext)(jvalue: JValue): EvaluatedJValue =
    jvalue match
    case JValue.JFalse => EvaluatedJValue.JBoolean(false)
    case JValue.JTrue => EvaluatedJValue.JBoolean(true)
    case JValue.JNull => EvaluatedJValue.JNull
    case JValue.JSelf => ctx.self()
    case JValue.JSuper => ctx.lookup("super")
    case JValue.JOuter => ctx.outer()
    case JValue.JString(str) => EvaluatedJValue.JString(str)
    case JValue.JNum(str) => EvaluatedJValue.JNum(str.toDouble)
    case JValue.JArray(elements) => EvaluatedJValue.JArray(elements.map(apply(ctx)))
    case JValue.JObject(members) =>
      ctx.makeObject(members)
    // case JValue.JObjectComprehension(preLocals, key, value, postLocals, forVar, inExpr, cond) => ???
    case JValue.JId(name) => ctx.lookup(name)
    case JValue.JGetField(loc, field) =>
      val obj = ctx.expectType[EvaluatedJValue.JObject](apply(ctx)(loc))
      obj
        .members()
        .getOrElse(field, ctx.error(s"object does not have field $field"))
        .evaluated
    case JValue.JIndex(loc, rawIndex, rawEndIndex, stride) =>
      apply(ctx)(loc) match
      case obj: EvaluatedJValue.JObject =>
        if !rawEndIndex.isNull then ctx.error("no end index allowed for object index")
        if !stride.isNull then ctx.error("no stride allowed for object index")
        val field = ctx.expectType[EvaluatedJValue.JString](apply(ctx)(rawIndex))
        obj.lookup(field.str)
      case arr: EvaluatedJValue.JArray =>
        val index = ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(rawIndex)).double.toInt
        val endIndex =
          if rawEndIndex.isNull then None
          else Some(ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(rawIndex)).double.toInt)
        arr.index(index, endIndex, None)
      case _ => ctx.error(s"expected object or array")
    case JValue.JApply(loc, positionalArgs, namedArgs) =>
      val fn = ctx.expectType[EvaluatedJValue.JFunction](apply(ctx)(loc))
      val numGivenArgs = positionalArgs.size + namedArgs.size
      if numGivenArgs > fn.params.size then
        ctx.error("to many arguments for function")
      val argMap = namedArgs.toMap
      val (_, argsCtx) = fn.params.foldLeft(positionalArgs -> ctx) {
        case ((positionalArgs, ctx), (argName, default)) =>
          val isGivenNamedArg = argMap.contains(argName)
          if positionalArgs.nonEmpty && isGivenNamedArg then
            ctx.error(s"both positional and named arg provided for argument $argName")
          else if positionalArgs.nonEmpty then
            (positionalArgs.tail, ctx.bind(argName, positionalArgs.head))
          else if isGivenNamedArg then
            (positionalArgs, ctx.bind(argName, argMap(argName)))
          else if default.isDefined then
            (positionalArgs, ctx.bind(argName, default.get))
          else
            ctx.error(s"missing argument $argName")
      }
      val functionCtx = argsCtx.functionCtx(fn)
      apply(functionCtx)(fn.body)
    case JValue.JBinaryOp(left, op, right) =>
      op match
      case JBinaryOperator.Op_+ =>
        val op1 = ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(left))
        val op2 = ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(right))
        EvaluatedJValue.JNum(op1.double + op2.double)

    case JValue.JUnaryOp(op, rawOperand) =>
      op match
      case JUnaryOperator.Op_! =>
        val operand = ctx.expectType[EvaluatedJValue.JBoolean](apply(ctx)(rawOperand))
        EvaluatedJValue.JBoolean(!operand.value)
      case JUnaryOperator.Op_+  =>
        val operand = ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(rawOperand)).double
        EvaluatedJValue.JNum(operand.toInt)
      case JUnaryOperator.Op_-  =>
        val operand = ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(rawOperand)).double
        EvaluatedJValue.JNum(-operand.toInt)
      case JUnaryOperator.Op_~  =>
        val operand = ctx.expectType[EvaluatedJValue.JNum](apply(ctx)(rawOperand)).double
        EvaluatedJValue.JNum(~operand.toInt)
    case JValue.JLocal(name, value, result) =>
      apply(ctx.bind(name, value))(result)
    // case JValue.JArrComprehension(comp: JValue, inExprs: Seq[JValue], cond: Option[JValue])
    case JValue.JFunction(params, body) =>
      EvaluatedJValue.JFunction(params, body)
    case JValue.JIf(rawCond, trueValue, elseValue) =>
      val cond = ctx.expectType[EvaluatedJValue.JBoolean](apply(ctx)(rawCond))
      if cond.value then
        apply(ctx)(trueValue)
      else
        apply(ctx)(elseValue)
    case JValue.JError(rawExpr) =>
      val msg = ctx.expectType[EvaluatedJValue.JString](apply(ctx)(rawExpr))
      ctx.error(msg.str)
    case JValue.JAssert(rawCond, rawMsg, expr) =>
      val cond = ctx.expectType[EvaluatedJValue.JBoolean](apply(ctx)(rawCond))
      if !cond.value then
        val msg = rawMsg.map(msg => ctx.expectType[EvaluatedJValue.JString](apply(ctx)(msg)).str)
        ctx.error(msg.getOrElse(s"assertion failed"))
      apply(ctx)(expr)
    case JValue.JImport(file) =>
      ctx.importFile(file)
    case JValue.JImportStr(file) =>
      ctx.importStr(file)
  /*
    case JValue.JArrayComprehension(
      forVar: String,
      forExpr: JValue,
      inExpr: JValue,
      cond: Option[JValue]
    )
  */
