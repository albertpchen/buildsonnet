package root

sealed trait EvaluationContext:
  def error(message: String): Nothing
  def lookup(id: String): EvaluatedJValue
  def objectCtx(): EvaluationContext
  def functionCtx(fn: EvaluatedJValue.JFunction): EvaluationContext
  def bind(id: String, value: JValue): EvaluationContext
  def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue): T
  def importFile(fileName: String): EvaluatedJValue
  def importStr(fileName: String): EvaluatedJValue.JString

object EvaluationContext:
  private class LazyValue(ctx: EvaluationContext, code: JValue):
    @scala.annotation.threadUnsafe
    lazy val evaluated: EvaluatedJValue = {
      eval(ctx)(code)
    }

  private case class Imp(
    val scope: Map[String, LazyValue],
    val stack: List[EvaluatedJValue.JFunction | EvaluatedJValue.JObject],
  ) extends EvaluationContext:
    def error(message: String): Nothing = throw new Exception(message)

    def lookup(id: String): EvaluatedJValue =
      scope.get(id).fold {
        error(s"no variable $id defined")
      }(_.evaluated)

    def objectCtx(): EvaluationContext = ???

    def functionCtx(fn: EvaluatedJValue.JFunction): EvaluationContext =
      new Imp(scope, fn +: stack)

    def bind(id: String, value: JValue): EvaluationContext =
      val newScope = scope + (id -> new LazyValue(this, value))
      new Imp(newScope, stack)

    def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue): T =
      if expr.isInstanceOf[T] then expr.asInstanceOf[T] else error("unexpected type")

    def importFile(fileName: String): EvaluatedJValue = ???
    def importStr(fileName: String): EvaluatedJValue.JString = ???

  def apply(): EvaluationContext = new Imp(Map.empty, List.empty)


enum EvaluatedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[EvaluatedJValue])
  case JObject(members: Seq[(String, EvaluatedJValue)])
  case JFunction(params: JParamList, body: JValue)

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
    case JValue.JSelf => ctx.lookup("self")
    case JValue.JSuper => ctx.lookup("super")
    case JValue.JOuter => ctx.lookup("$")
    case JValue.JString(str) => EvaluatedJValue.JString(str)
    case JValue.JNum(str) => EvaluatedJValue.JNum(str.toDouble)
    case JValue.JArray(elements) => EvaluatedJValue.JArray(elements.map(apply(ctx)))
    case JValue.JObject(members) =>
      val objCtx = ctx.objectCtx()
      EvaluatedJValue.JObject(members.flatMap {
        case JObjMember.JLocal(name, value) =>
          objCtx.bind(name, value)
          None
        case JObjMember.JField(key, isHidden, value) =>
          val evaluatedKey = objCtx.expectType[EvaluatedJValue.JString](apply(objCtx)(key))
          Some(evaluatedKey.str -> apply(objCtx)(value))
        case JObjMember.JAssert(cond, expr) =>
          val evaluatedCond = objCtx.expectType[EvaluatedJValue.JBoolean](apply(objCtx)(cond))
          val msgOpt = expr.map(e => objCtx.expectType[EvaluatedJValue.JString](apply(objCtx)(e)))
          if !evaluatedCond.value then
            objCtx.error(msgOpt.fold("assertion failed")(_.str))
          None
      })
    // case JValue.JObjectComprehension(preLocals, key, value, postLocals, forVar, inExpr, cond) => ???
    case JValue.JId(name) => ctx.lookup(name)
    case JValue.JGetField(loc, field) =>
      val obj = ctx.expectType[EvaluatedJValue.JObject](apply(ctx)(loc))
      obj.lookup(field)
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
