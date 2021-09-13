package root

sealed trait LazyValue:
  def evaluated(): EvaluatedJValue

sealed trait EvaluationContext:
  def error(message: String): Nothing
  def lookupScope(id: String): LazyValue
  def objectCtx(): EvaluationContext
  def bind(id: String, value: JValue): Unit
  def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue, expected: JType[T]): T

enum EvaluatedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[EvaluatedJValue])
  case JObject(members: Seq[(String, EvaluatedJValue)])
  case JFunction(params: JParamList, body: JValue)
  case JError(msg: String)

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
    case JValue.JSelf => ctx.lookupScope("self").evaluated()
    case JValue.JSuper => ctx.lookupScope("super").evaluated()
    case JValue.JOuter => ctx.lookupScope("$").evaluated()
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
          val evaluatedKey = objCtx.expectType(apply(objCtx)(key), JType.JString)
          Some(evaluatedKey.str -> apply(objCtx)(value))
        case JObjMember.JAssert(cond, expr) =>
          val evaluatedCond = objCtx.expectType(apply(objCtx)(cond), JType.JBoolean)
          val msgOpt = expr.map(e => objCtx.expectType(apply(objCtx)(e), JType.JString))
          if !evaluatedCond.value then
            objCtx.error(msgOpt.fold("assertion failed")(_.str))
          None
      })
    case JValue.JObjectComprehension(preLocals, key, value, postLocals, forVar, inExpr, cond) => ???
    case JValue.JId(name) => ctx.lookupScope(name).evaluated()
    case JValue.JGetField(loc, field) =>
      val obj = ctx.expectType(apply(ctx)(loc), JType.JObject)
      obj.lookup(field)
    case JValue.JIndex(loc, rawIndex, rawEndIndex, stride) =>
      apply(ctx)(loc) match
      case obj: EvaluatedJValue.JObject =>
        if !rawEndIndex.isNull then ctx.error("no end index allowed for object index")
        if !stride.isNull then ctx.error("no stride allowed for object index")
        val field = ctx.expectType(apply(ctx)(rawIndex), JType.JString)
        obj.lookup(field.str)
      case arr: EvaluatedJValue.JArray =>
        val index = ctx.expectType(apply(ctx)(rawIndex), JType.JNum).double.toInt
        val endIndex =
          if rawEndIndex.isNull then None
          else Some(ctx.expectType(apply(ctx)(rawIndex), JType.JNum).double.toInt)
        arr.index(index, endIndex, None)
      case _ => ctx.error(s"expected object or array")
    case JValue.JApply(loc, positionalArgs, namedArgs) =>
      val fn = ctx.expectType(apply(ctx)(loc), JType.JFunction)
      ???
  /*
    case JValue.JBinaryOp(left: JValue, op: JBinaryOperator, right: JValue)
    case JValue.JUnaryOp(op: JUnaryOperator, expr: JValue)
    case JValue.JLocal(name: String, value: JValue, result: JValue)
    case JValue.JArrComprehension(comp: JValue, inExprs: Seq[JValue], cond: Option[JValue])
    case JValue.JFunction(params: JParamList, body: JValue)
    case JValue.JIf(cond: JValue, trueValue: JValue, elseValue: JValue)
    case JValue.JError(expr: JValue)
    case JValue.JAssert(cond: JValue, expr: Option[JValue], result: JValue)
    case JValue.JImport(file: String)
    case JValue.JImportStr(file: String)
    case JValue.JArrayComprehension(
      forVar: String,
      forExpr: JValue,
      inExpr: JValue,
      cond: Option[JValue]
    )
  */
