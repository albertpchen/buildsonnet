package root

sealed trait LazyValue:
  def evaluated(): EvaluatedJValue

sealed trait EvaluationContext:
  def error(message: String): Nothing
  def lookupScope(id: String): LazyValue
  def objectCtx(): EvaluationContext
  def bind(id: String, value: JValue): Unit
  def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue, expected: JType[T]): T

enum JType[T <: EvaluatedJValue]:
  case JBoolean extends JType[EvaluatedJValue.JBoolean]
  case JNumber extends JType[EvaluatedJValue.JBoolean]
  case JString extends JType[EvaluatedJValue.JString]
  case JNull extends JType[EvaluatedJValue.JNull.type]
  case JArray extends JType[EvaluatedJValue.JArray]
  case JObject extends JType[EvaluatedJValue.JObject]

enum EvaluatedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[EvaluatedJValue])
  case JObject(members: Seq[(String, EvaluatedJValue)])
  case JFunction(params: JParamList, body: JValue)
  case JError(msg: String)

object eval:
  def apply(ctx: EvaluationContext)(jvalue: JValue): EvaluatedJValue =
    import JValue._
    jvalue match
    case JFalse => EvaluatedJValue.JBoolean(false)
    case JTrue => EvaluatedJValue.JBoolean(true)
    case JNull => EvaluatedJValue.JNull
    case JSelf => ctx.lookupScope("self").evaluated()
    case JSuper => ctx.lookupScope("super").evaluated()
    case JOuter => ctx.lookupScope("$").evaluated()
    case JString(str) => EvaluatedJValue.JString(str)
    case JNum(str) => EvaluatedJValue.JNum(str.toDouble)
    case JArray(elements) => EvaluatedJValue.JArray(elements.map(apply(ctx)))
    case JObject(members) =>
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
    case JObjectComprehension(preLocals, comp, postLocals, inExprs, cond) => ???
    case JId(name) => ctx.lookupScope(name).evaluated()
    case JGetField(loc, field) =>
      // val obj = evaluateLazy(loc)
      // obj.lookup(field)
      ???
    case JIndex(loc, index) =>
      // val obj = evaluateLazy(loc)
      // obj.lookup(field)
      ???
    case JApply(loc, positionalArgs, namedArgs) =>
      // val obj = evaluateLazy(loc)
      // obj.lookup(field)
      ???
      /*
    case JBinaryOp(left: JValue, op: JBinaryOperator, right: JValue)
    case JUnaryOp(op: JUnaryOperator, expr: JValue)
    case JLocal(name: String, value: JValue, result: JValue)
    case JArrComprehension(comp: JValue, inExprs: Seq[JValue], cond: Option[JValue])
    case JFunction(params: JParamList, body: JValue)
    case JIf(cond: JValue, trueValue: JValue, elseValue: JValue)
    case JError(expr: JValue)
    case JAssert(cond: JValue, expr: Option[JValue], result: JValue)
    case JImport(file: String)
    case JImportStr(file: String)
    case JArrayComprehension(
      forVar: String,
      forExpr: JValue,
      inExpr: JValue,
      cond: Option[JValue]
    )*/
