package root

sealed trait LazyValue:
  def evaluated(): EvaluatedJValue


sealed trait EvaluationContext:
  def error(message: String): Nothing
  def lookupScope(id: String): LazyValue
  def objectCtx(): EvaluationContext
  def functionCtx(): EvaluationContext
  def bind(id: String, value: JValue): Unit
  def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue, expected: JType[T]): T
  def importFile(fileName: String): EvaluatedJValue
  def importStr(fileName: String): EvaluatedJValue.JString


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
    // case JValue.JObjectComprehension(preLocals, key, value, postLocals, forVar, inExpr, cond) => ???
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
      val argMap = namedArgs.toMap
      val numGivenArgs = positionalArgs.size + namedArgs.size
      if numGivenArgs > fn.params.size then
        ctx.error("to many arguments for function")
      else if numGivenArgs < fn.params.size then
        ctx.error("to few arguments for function")
      val (_, givenArgs) = fn.params.foldLeft(positionalArgs -> Seq.empty[(String, JValue)]) {
        case ((positionalArgs, result), (argName, default)) =>
          val isGivenNamedArg = argMap.contains(argName)
          if positionalArgs.nonEmpty && isGivenNamedArg then
            ctx.error(s"both positional and named arg provided for argument $argName")
          else if positionalArgs.nonEmpty then
            (positionalArgs.tail, (argName, positionalArgs.head) +: result)
          else if isGivenNamedArg then
            (positionalArgs, (argName, argMap(argName)) +: result)
          else if default.isDefined then
            (positionalArgs, (argName, default.get) +: result)
          else
            ctx.error(s"missing argument $argName")
      }
      val functionCtx = ctx.functionCtx()
      givenArgs.foreach(functionCtx.bind(_, _))
      apply(functionCtx)(fn.body)
    // case JValue.JBinaryOp(left: JValue, op: JBinaryOperator, right: JValue)
    // case JValue.JUnaryOp(op: JUnaryOperator, expr: JValue)
    case JValue.JLocal(name, value, result) =>
      ctx.bind(name, value)
      apply(ctx)(result)
    // case JValue.JArrComprehension(comp: JValue, inExprs: Seq[JValue], cond: Option[JValue])
    case JValue.JFunction(params, body) =>
      EvaluatedJValue.JFunction(params, body)
    case JValue.JIf(rawCond, trueValue, elseValue) =>
      val cond = ctx.expectType(apply(ctx)(rawCond), JType.JBoolean)
      if cond.value then
        apply(ctx)(trueValue)
      else
        apply(ctx)(elseValue)
    case JValue.JError(rawExpr) =>
      val msg = ctx.expectType(apply(ctx)(rawExpr), JType.JString)
      ctx.error(msg.str)
    case JValue.JAssert(rawCond, rawMsg, expr) =>
      val cond = ctx.expectType(apply(ctx)(rawCond), JType.JBoolean)
      if !cond.value then
        val msg = rawMsg.map(msg => ctx.expectType(apply(ctx)(msg), JType.JString).str)
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
