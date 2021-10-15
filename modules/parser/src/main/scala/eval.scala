package root

import concurrent.{ExecutionContext, Future}

sealed trait EvaluatedJObject:
  def ctx: ObjectEvaluationContext
  protected def cache: collection.Map[String, LazyObjectValue]

  def withCtx(newCtx: () => ObjectEvaluationContext): EvaluatedJObject

  def lookup(src: Source, field: String): LazyValue =
    cache.getOrElse(field, ctx.error(src, s"object missing field $field"))

  def lookupOpt(src: Source, field: String): Option[LazyValue] =
    cache.get(field)

  private lazy val _members: collection.Map[String, LazyObjectValue] =
    val members = new collection.mutable.HashMap[String, LazyObjectValue]()
    given ExecutionContext = ctx.executionContext
    ctx.superChain.foreach { s =>
      s.members().foreach { (k, v) =>
        val newValue =
          if members.contains(k) && members(k).isHidden && !v.isHidden then
            v.withHidden(members(k).isHidden)
          else
            v
        members(k) = newValue
      }
      members
    }
    cache.foreach { (k, v) =>
      val newValue =
        if members.contains(k) && members(k).isHidden && !v.isHidden then
          v.withHidden(members(k).isHidden)
        else
          v
      members(k) = newValue
    }
    members

  def members(): collection.Map[String, LazyObjectValue] =
    _members

object EvaluatedJObject:
  def comprehension(
    ctxThunk: () => ObjectEvaluationContext,
    arrayElementsFuture: Seq[(String, EvaluatedJValue)],
    forVar: String,
    value: JValue,
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()
    def withCtx(newCtx: () => ObjectEvaluationContext) =
      comprehension(newCtx, arrayElementsFuture, forVar, value)

    lazy val cache = {
      given ExecutionContext = ctx.executionContext
      arrayElementsFuture.map { (key, e) =>
        val valueCtx = ctx.bindEvaluated(forVar, e).withStackEntry(StackEntry.objectField(value.src))
        key -> LazyValue(valueCtx, value, false)
      }.toMap
    }

  def static(
    ctxThunk: () => ObjectEvaluationContext,
    staticMembers: EvaluationContext => Map[String, EvaluatedJValue | LazyObjectValue],
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()

    def withCtx(newCtx: () => ObjectEvaluationContext) = static(newCtx, staticMembers)

    lazy val cache = staticMembers(ctx).view.mapValues {
      case value: EvaluatedJValue => LazyValue.strictObject(value, false)
      case value: LazyObjectValue => value
    }.toMap

  def apply(
    ctxThunk: () => ObjectEvaluationContext,
    cacheFn: EvaluationContext => collection.Map[String, LazyObjectValue],
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()

    def withCtx(newCtx: () => ObjectEvaluationContext) =
      apply(newCtx, cacheFn)

    lazy val cache = cacheFn(ctx)

final class EvaluatedJFunctionParameters(
  val src: Source,
  val positionalArgs: Seq[EvaluatedJValue],
  val namedArgs: Seq[(String, EvaluatedJValue)],
)

sealed trait EvaluatedJValue extends HasSource:
  import EvaluatedJValue._
  import concurrent.{Await, duration}
  def isNull: Boolean =
    this match
    case _: JNull => true
    case _ => false

  def await(ctx: EvaluationContext): Unit =
    given ExecutionContext = ctx.executionContext
    this match
    case value: JFuture =>
      Await.result(value.future, duration.Duration.Inf).await(ctx)
    case value: JArray =>
      value.elements.foreach(_.await(ctx))
    case value: JObject =>
      value.members().values.foreach(_.evaluated.await(ctx))
    case _ =>

  def manifestFuture(ctx: EvaluationContext): Future[ManifestedJValue] =
    given ExecutionContext = ctx.executionContext
    this match
    case value: JBoolean => Future(ManifestedJValue.JBoolean(value.value))
    case value: JNull => Future(ManifestedJValue.JNull)
    case value: JString => Future(ManifestedJValue.JString(value.str))
    case value: JNum => Future(ManifestedJValue.JNum(value.double))
    case value: JArray =>
      Future.sequence(value.elements.map(_.manifestFuture(ctx))).map(ManifestedJValue.JArray(_))
    case obj: JObject =>
      val members = obj.members()
      val futures = members.collect {
        case (key, value) if !value.isHidden => value.evaluated.manifestFuture(ctx).map(key -> _)
      }
      Future.sequence(futures).map(members => ManifestedJValue.JObject(members.toMap))
    case f: JFuture => f.future.map(_.manifest(ctx))
    case expr => ctx.error(expr.src, s"cannot manifest ${EvaluationContext.typeString(expr)}")

  def manifest(ctx: EvaluationContext): ManifestedJValue =
    concurrent.Await.result(manifestFuture(ctx), duration.Duration.Inf)

object EvaluatedJValue:
  case class JBoolean(src: Source, value: Boolean) extends EvaluatedJValue
  case class JNull(src: Source) extends EvaluatedJValue
  case class JString(src: Source, str: String) extends EvaluatedJValue
  case class JNum(src: Source, double: Double) extends EvaluatedJValue
  case class JPath(src: Source, path: java.nio.file.Path) extends EvaluatedJValue
  case class JJob(src: Source, desc: JobDescription, stdout: String, stderr: String, outputs: Seq[JPath], exitCode: Int) extends EvaluatedJValue
  case class JArray(src: Source, elements: Seq[EvaluatedJValue]) extends EvaluatedJValue
  /** needs to handle:
    *
    * - simple lookup, defined in current object
    * - super lookup (dynamic), not in current object, but in super object
    * - self lookup (dynamic), lookup in current object, possibly in super
    */
  case class JObject(src: Source, imp: EvaluatedJObject) extends EvaluatedJValue
  case class JFunction(src: Source, numParams: Int, fn: (EvaluationContext, EvaluatedJFunctionParameters) => EvaluatedJValue) extends EvaluatedJValue
  case class JFuture(src: Source, future: Future[EvaluatedJValue.JNow]) extends EvaluatedJValue

  extension [T <: EvaluatedJValue](future: concurrent.Future[T])
    inline def toJValue(using ctx: concurrent.ExecutionContext): EvaluatedJValue =
      inline future match
      case future: concurrent.Future[EvaluatedJValue.JNow] => EvaluatedJValue.JFuture(Source.empty, future)
      case future: concurrent.Future[EvaluatedJValue.JFuture] => EvaluatedJValue.JFuture(Source.empty, future.flatMap(_.future))
      case future: concurrent.Future[EvaluatedJValue] =>
        EvaluatedJValue.JFuture(Source.empty, future.flatMap {
          case now: EvaluatedJValue.JNow => concurrent.Future(now)
          case future: EvaluatedJValue.JFuture => future.future
        })

  type JNow = 
    JBoolean
    | JNull
    | JString
    | JNum
    | JPath
    | JJob
    | JArray
    | JObject
    | JFunction

  extension (obj: EvaluatedJValue.JObject)
    def ctx: ObjectEvaluationContext = obj.imp.ctx

    def withCtx(ctx: () => ObjectEvaluationContext): EvaluatedJValue.JObject =
      obj.copy(imp = obj.imp.withCtx(ctx))

    def lookup(src: Source, field: String): EvaluatedJValue =
      obj.imp.lookup(src, field).evaluated

    def members(): collection.Map[String, LazyObjectValue] =
      obj.imp.members()

  extension (arr: EvaluatedJValue.JArray)
    def index(src: Source, ctx: EvaluationContext, idx: Int, endIdxOpt: Option[Int], strideOpt: Option[Int]): EvaluatedJValue =
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

  extension (value: EvaluatedJValue)
    def structuralEquals(other: EvaluatedJValue)(using ExecutionContext): Future[Boolean] =
      (value, other) match
      case (op1: JBoolean, op2: JBoolean) => Future(op1.value == op2.value)
      case (op1: JNull, op2: JNull) => Future(true)
      case (op1: JString, op2: JString) => Future(op1.str == op2.str)
      case (op1: JNum, op2: JNum) => Future(op1.double == op2.double)
      case (op1: JPath, op2: JPath) => Future(op1.path == op2.path)
      case (op1: JJob, op2: JJob) => Future(op1 eq op2)
      case (op1: JArray, op2: JArray) =>
        if op1.elements.size == op2.elements.size then
          op1.elements.zip(op2.elements).foldLeft(Future(true)) {
            case (result, (op1, op2)) => result.flatMap { res =>
              if res then
                op1.structuralEquals(op2)
              else
                result
            }
          }
        else
          Future(false)
      case (op1: JFunction, op2: JFunction) => Future(op1 eq op2)
      case (op1: JObject, op2: JObject) =>
        val members1 = op1.members()
        val members2 = op2.members()
        if members1.keys == members2.keys then
          members1.keys.foldLeft(Future(true)) {
            case (result, key) => result.flatMap { res =>
              if res then
                members1(key).evaluated.structuralEquals(members2(key).evaluated)
              else
                result
            }
          }
        else Future(false)
      case (op1: JFuture, op2) => op1.future.flatMap(_.structuralEquals(op2))
      case (op1, op2: JFuture) => op2.future.flatMap(op1.structuralEquals)
      case (op1, op2) => Future(false)

def manifest(ctx: EvaluationContext)(jvalue: JValue): Either[EvaluationError, ManifestedJValue] =
  try
    val evaluated = evalUnsafe(ctx)(jvalue)
    Right(evaluated.manifest(ctx))
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
        (positionalArgs.tail, ctx.bindEvaluated(argName, positionalArgs.head))
      else if isGivenNamedArg then
        (positionalArgs, ctx.bindEvaluated(argName, argMap(argName)))
      else if default.isDefined then
        (positionalArgs, ctx.bindWithCtx(argName, defCtx, default.get))
      else
        applyCtx.error(params.src, s"missing argument $argName")
  }
  val functionCtx = argsCtx.functionCtx(fnSrc)
  evalUnsafe(functionCtx)(body)

def evalUnsafe(ctx: EvaluationContext)(jvalue: JValue): EvaluatedJValue =
  import concurrent.Future
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
  case JValue.JObject(src, rawMembers) =>
    given ExecutionContext = ctx.executionContext
    var objCtx: ObjectEvaluationContext = null
    val members = rawMembers.collect { case JObjMember.JField(src, rawKey, plus, isHidden, value) =>
      ctx.expectFieldName(rawKey).map {
        case _: EvaluatedJValue.JNull => None
        case expr: EvaluatedJValue.JString =>
          val key = expr.str
          val valueCtx = () => objCtx.withStackEntry(StackEntry.objectField(value.src))
          val newValue =
            if plus then
              JValue.JBinaryOp(
                src,
                JValue.JGetField(src, JValue.JSuper(src), key),
                JBinaryOperator.Op_+,
                value
              )
            else
              value
          Some((key, newValue, isHidden))
      }
    }
    Future.sequence(members).flatMap { members =>
      val obj = EvaluatedJValue.JObject(
        src,
        EvaluatedJObject(
          () => objCtx,
          ctx => members.flatten.map((key, value, hidden) => key -> LazyValue(ctx, value, hidden)).toMap,
        )
      )
      objCtx = rawMembers.foldLeft(ctx.objectCtx(obj)) {
        case (ctx, local: JObjMember.JLocal) => ctx.bind(local.name, local.value)
        case (ctx, _) => ctx
      }

      // evaluate asserts inside object
      val asserts = rawMembers.collect {
        case JObjMember.JAssert(src, rawCond, rawMsg) =>
          val cond = objCtx.expectBoolean(rawCond)
          val condWithMsg = rawMsg.fold(cond.map(_ -> Option.empty[String])) { msg =>
            cond.zip(objCtx.expectString(msg).map(m => Some(m.str)))
          }
          condWithMsg.map { (cond, msgOpt) =>
            if !cond.value then objCtx.error(src, msgOpt.getOrElse("object assertion failed"))
          }
      }
      Future.sequence(asserts).map(_ => obj)
    }.toJValue

  case JValue.JObjectComprehension(src, preLocals, rawKey, value, postLocals, forVar, inExpr, condOpt) =>
    given ExecutionContext = ctx.executionContext
    val arrElements: Future[Seq[(String, EvaluatedJValue)]] = ctx.expectArray(inExpr).flatMap { arr =>
      if condOpt.isDefined then
        val cond = condOpt.get
        Future.sequence(arr.elements.map { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          forCtx.expectFieldName(rawKey).zip(ctx.expectBoolean(cond)).map {
            case (_: EvaluatedJValue.JNull, _) => None
            case (key: EvaluatedJValue.JString, cond) => Option.when(cond.value) {
              key.str -> e
            }
          }
        }).map(_.flatten)
      else
        Future.sequence(arr.elements.map { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          forCtx.expectFieldName(rawKey).map {
            case _: EvaluatedJValue.JNull => None
            case key: EvaluatedJValue.JString => Some(key.str -> e)
          }
        }).map(_.flatten)
    }
    arrElements.map { arrElements =>
      var objCtx: ObjectEvaluationContext = null
      val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
        src,
        EvaluatedJObject.comprehension(
          () => objCtx,
          arrElements,
          forVar,
          value,
        )
      )
      objCtx = preLocals.foldLeft(ctx.objectCtx(obj)) { (ctx, local) =>
        ctx.bind(local.name, local.value)
      }
      objCtx = postLocals.foldLeft(objCtx) { (ctx, local) =>
        ctx.bind(local.name, local.value)
      }
      obj
    }.toJValue
  case JValue.JId(src, name) => ctx.lookup(src, name).evaluated
  case JValue.JGetField(src, loc, field) =>
    given ExecutionContext = ctx.executionContext
    ctx.expectType[EvaluatedJValue.JObject | EvaluatedJValue.JJob | EvaluatedJValue.JPath](loc).map {
      case o: EvaluatedJValue.JObject =>
        o
          .members()
          .getOrElse(field, ctx.error(loc.src, s"object does not have field $field"))
          .evaluated
      case p: EvaluatedJValue.JPath =>
        field match
        case "name" => EvaluatedJValue.JString(src, p.path.toString)
        case _ => ctx.error(loc.src, s"path does not have field $field")
      case j: EvaluatedJValue.JJob =>
        field match
        case "stdout" => EvaluatedJValue.JString(src, j.stdout)
        case "stderr" => EvaluatedJValue.JString(src, j.stderr)
        case "outputs" => EvaluatedJValue.JArray(src, j.outputs)
        case "exitCode" => EvaluatedJValue.JNum(src, j.exitCode.toDouble)
        case _ => ctx.error(loc.src, s"job does not have field $field")
    }.toJValue
  case JValue.JIndex(src, loc, rawIndex) =>
    given ExecutionContext = ctx.executionContext
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](evalUnsafe(ctx)(loc)).flatMap {
    case obj: EvaluatedJValue.JObject =>
      ctx.expectString(rawIndex).map { field =>
        obj.lookup(src, field.str)
      }
    case arr: EvaluatedJValue.JArray =>
      ctx.expectNum(rawIndex).map { num =>
        val idx = num.double.toInt
        if idx >= arr.elements.size then
          ctx.error(src, s"index $idx out of bounds for length ${arr.elements.size}")
        arr.elements(idx)
      }
    }.toJValue
  case JValue.JSlice(src, loc, rawIndex, rawEndIndex, rawStride) =>
    given concurrent.ExecutionContext = ctx.executionContext
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](loc).flatMap {
      case obj: EvaluatedJValue.JObject =>
        ctx.error(src, "no end index or stride allowed for object index")
      case arr: EvaluatedJValue.JArray =>
        val index = ctx.expectNum(rawIndex)
        val none = Future(Option.empty[Int])
        val endIndex = rawEndIndex.fold(none)(num => ctx.expectNum(num).map(n => Some(n.double.toInt)))
        val stride = rawStride.fold(none)(num => ctx.expectNum(num).map(n => Some(n.double.toInt)))
        index.zip(endIndex).zip(stride).map { case ((index, endIndex), stride) =>
          arr.index(src, ctx, index.double.toInt, endIndex, stride)
        }
    }.toJValue
  case JValue.JApply(src, loc, positionalArgs, namedArgs) =>
    given concurrent.ExecutionContext = ctx.executionContext
    ctx.expectFunction(loc).map { fn =>
      val params = EvaluatedJFunctionParameters(
        src,
        positionalArgs.map(evalUnsafe(ctx)),
        namedArgs.map((n, a) => n -> evalUnsafe(ctx)(a)),
      )
      fn.fn(ctx, params)
    }.toJValue
  case JValue.JBinaryOp(src, left, op, right) =>
    given concurrent.ExecutionContext = ctx.executionContext
    op match
    case JBinaryOperator.Op_+ =>
      type PlusOperand = EvaluatedJValue.JString | EvaluatedJValue.JNum | EvaluatedJValue.JObject | EvaluatedJValue.JArray
      ctx.expectType[PlusOperand](left).zip(ctx.expectType[PlusOperand](right)).map {
        case (op1: EvaluatedJValue.JString, op2) =>
          EvaluatedJValue.JString(src, op1.str + Std.toStringImp(ctx, op2.src, op2).str)
        case (op1, op2: EvaluatedJValue.JString) =>
          EvaluatedJValue.JString(src, Std.toStringImp(ctx, op1.src, op1).str + op2)
        case (op1: EvaluatedJValue.JNum, op2: EvaluatedJValue.JNum) =>
          EvaluatedJValue.JNum(src, op1.double + op2.double)
        case (op1: EvaluatedJValue.JObject, op2: EvaluatedJValue.JObject) =>
          var parentCtx: ObjectEvaluationContext = null
          val parent: EvaluatedJValue.JObject = op1.withCtx(ctx = () => parentCtx)
          var resultCtx: ObjectEvaluationContext = null
          val result: EvaluatedJValue.JObject = op2.withCtx(ctx = () => resultCtx)
          resultCtx = op2.ctx.withSelf(result).withSuper(parent)
          parentCtx = op1.ctx.withSelf(result)
          result
        case (op1: EvaluatedJValue.JArray, op2: EvaluatedJValue.JArray) =>
          EvaluatedJValue.JArray(src, op1.elements ++ op2.elements)
        case (op1, op2) =>
          ctx.error(src, s"invalid operand types, expected two numbers, arrays, or objects, or one string")
      }.toJValue
    case JBinaryOperator.Op_- =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JNum(src, left.double - right.double)
      }.toJValue
    case JBinaryOperator.Op_* =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JNum(src, left.double * right.double)
      }.toJValue
    case JBinaryOperator.Op_/ =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JNum(src, left.double / right.double)
      }.toJValue
    case JBinaryOperator.Op_< =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JBoolean(src, left.double < right.double)
      }.toJValue
    case JBinaryOperator.Op_<= =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JBoolean(src, left.double <= right.double)
      }.toJValue
    case JBinaryOperator.Op_> =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JBoolean(src, left.double > right.double)
      }.toJValue
    case JBinaryOperator.Op_>= =>
      ctx.expectNum(left).zip(ctx.expectNum(right)).map { (left, right) =>
        EvaluatedJValue.JBoolean(src, left.double >= right.double)
      }.toJValue
    case JBinaryOperator.Op_>> =>
      ctx.expectNum(right).zip(ctx.expectNum(left)).map { (right, left) =>
        val rhs = right.double.toLong
        val shamt =
          if rhs >= 0 then
            rhs % 64
          else
            ctx.error(right.src, s"shift amount cannot be negative, got $rhs")
        EvaluatedJValue.JNum(src, (left.double.toLong >> shamt).toDouble)
      }.toJValue
    case JBinaryOperator.Op_<< =>
      ctx.expectNum(right).zip(ctx.expectNum(left)).map { (right, left) =>
        val rhs = right.double.toLong
        val shamt =
          if rhs >= 0 then
            rhs % 64
          else
            ctx.error(right.src, s"shift amount cannot be negative, got $rhs")
        EvaluatedJValue.JNum(src, (left.double.toLong << shamt).toDouble)
      }.toJValue
    case JBinaryOperator.Op_in =>
      ctx.expectString(left).zip(ctx.expectObject(right)).map { (left, right) =>
        EvaluatedJValue.JBoolean(src, right.members().contains(left.str))
      }.toJValue
    case JBinaryOperator.Op_== =>
      evalUnsafe(ctx)(left).structuralEquals(evalUnsafe(ctx)(right)).map(EvaluatedJValue.JBoolean(src, _)).toJValue
    case JBinaryOperator.Op_!= =>
      evalUnsafe(ctx)(left).structuralEquals(evalUnsafe(ctx)(right)).map(n => EvaluatedJValue.JBoolean(src, !n)).toJValue

  case JValue.JUnaryOp(src, op, rawOperand) =>
    given concurrent.ExecutionContext = ctx.executionContext
    op match
    case JUnaryOperator.Op_! =>
      ctx.expectBoolean(rawOperand).map { operand =>
        EvaluatedJValue.JBoolean(src, !operand.value)
      }.toJValue
    case JUnaryOperator.Op_+ => ctx.expectNum(rawOperand).toJValue
    case JUnaryOperator.Op_- =>
      ctx.expectNum(rawOperand).map { operand =>
        EvaluatedJValue.JNum(src, -operand.double)
      }.toJValue
    case JUnaryOperator.Op_~  =>
      ctx.expectNum(rawOperand).map { operand =>
        EvaluatedJValue.JNum(src, (~operand.double.toLong).toDouble)
      }.toJValue
  case JValue.JLocal(_, name, value, result) =>
    evalUnsafe(ctx.bind(name, value))(result)
  case JValue.JFunction(src, params, body) =>
    EvaluatedJValue.JFunction(src, params.size, applyArgs(ctx, src, params, body))
  case JValue.JIf(src, rawCond, trueValue, elseValue) =>
    given concurrent.ExecutionContext = ctx.executionContext
    ctx.expectBoolean(rawCond).map { cond =>
      if cond.value then
        evalUnsafe(ctx)(trueValue)
      else
        elseValue.fold(EvaluatedJValue.JNull(src))(evalUnsafe(ctx))
    }.toJValue
  case JValue.JError(src, rawExpr) =>
    given concurrent.ExecutionContext = ctx.executionContext
    ctx.expectString(rawExpr).map { msg =>
      ctx.error(src, msg.str): EvaluatedJValue.JString
    }.toJValue
  case JValue.JAssert(src, rawCond, rawMsg, expr) =>
    given concurrent.ExecutionContext = ctx.executionContext
    val msg = rawMsg.fold(Future(Option.empty[String])) { msg => ctx.expectString(msg).map(m => Some(m.str)) }
    ctx.expectBoolean(rawCond).zip(msg).map { (cond, msgOpt) =>
      if !cond.value then
        ctx.error(src, msgOpt.getOrElse(s"assertion failed"))
      evalUnsafe(ctx)(expr)
    }.toJValue
  case JValue.JImport(src, file) => ctx.`import`(src, file)
  case JValue.JImportStr(src, file) => ctx.importStr(src, file)
  case JValue.JArrayComprehension(src, forVar, forExpr, inExpr, condOpt) =>
    given concurrent.ExecutionContext = ctx.executionContext
    if condOpt.isDefined then
      val cond = condOpt.get
      ctx.expectArray(inExpr)
        .flatMap { array =>
          Future.sequence(array.elements.map { e =>
            val forCtx = ctx.bindEvaluated(forVar, e)
            forCtx.expectBoolean(cond).map { cond =>
              Option.when(cond.value) {
                evalUnsafe(forCtx)(forExpr)
              }
            }
          })
        }
        .map(elements => EvaluatedJValue.JArray(src, elements.flatten))
        .toJValue
    else
      ctx.expectArray(inExpr).map { array =>
        EvaluatedJValue.JArray(src, array.elements.map { e =>
          evalUnsafe(ctx.bindEvaluated(forVar, e))(forExpr)
        })
      }.toJValue
