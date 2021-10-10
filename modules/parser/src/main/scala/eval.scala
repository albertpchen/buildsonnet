package root

import concurrent.{ExecutionContext, Future}

sealed trait EvaluatedJObject:
  def ctx: ObjectEvaluationContext
  protected def cache: Future[collection.Map[String, LazyObjectValue]]

  def withCtx(newCtx: () => ObjectEvaluationContext): EvaluatedJObject

  def lookup(src: Source, field: String): Future[LazyValue] =
    given ExecutionContext = ctx.executionContext
    cache.map(_.getOrElse(field, ctx.error(src, s"object missing field $field")))

  def lookupOpt(src: Source, field: String): Future[Option[LazyValue]] =
    given ExecutionContext = ctx.executionContext
    cache.map(_.get(field))

  private lazy val _members: Future[collection.Map[String, LazyObjectValue]] =
    val members = new collection.mutable.HashMap[String, LazyObjectValue]()
    given ExecutionContext = ctx.executionContext
    val superMembers = ctx.superChain.foldLeft(Future(members)) { (members, s) =>
      members.zip(s.members()).map { (members, sMembers) =>
        sMembers.foreach { (k, v) =>
          val newValue =
            if members.contains(k) && members(k).isHidden && !v.isHidden then
              v.withHidden(members(k).isHidden)
            else
              v
          members(k) = newValue
        }
        members
      }
    }
    superMembers.zip(cache).map { (members, cache) =>
      cache.foreach { (k, v) =>
        val newValue =
          if members.contains(k) && members(k).isHidden && !v.isHidden then
            v.withHidden(members(k).isHidden)
          else
            v
        members(k) = newValue
      }
      members
    }

  def members(): Future[collection.Map[String, LazyObjectValue]] =
    _members

object EvaluatedJObject:
  def comprehension(
    ctxThunk: () => ObjectEvaluationContext,
    arrayElementsFuture: Future[Seq[(String, EvaluatedJValue)]],
    forVar: String,
    value: JValue,
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()
    def withCtx(newCtx: () => ObjectEvaluationContext) =
      comprehension(newCtx, arrayElementsFuture, forVar, value)

    lazy val cache = {
      given ExecutionContext = ctx.executionContext
      arrayElementsFuture.map(_.map { (key, e) =>
        val valueCtx = ctx.bindEvaluated(forVar, e).withStackEntry(StackEntry.objectField(ctx.file, value.src))
        key -> LazyValue(valueCtx, value, false)
      }.toMap)
    }

  def apply(
    ctxThunk: () => ObjectEvaluationContext,
    rawMembers: Seq[JObjMember.JField],
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()

    def withCtx(newCtx: () => ObjectEvaluationContext) =
      apply(newCtx, rawMembers)

    lazy val cache = {
      given ExecutionContext = ctx.executionContext
      val futures = rawMembers.map { case JObjMember.JField(src, rawKey, plus, isHidden, value) =>
        ctx.expectFieldName(rawKey).map {
          case _: EvaluatedJValue.JNull => None
          case expr: EvaluatedJValue.JString =>
            val key = expr.str
            val valueCtx = ctx.withStackEntry(StackEntry.objectField(ctx.file, value.src))
            val lazyValue = if plus then
              LazyValue(
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
              LazyValue(valueCtx, value, isHidden)
            Some(key -> lazyValue)
        }
      }
      Future.sequence(futures).map(_.flatten.toMap)
    }

final class EvaluatedJFunctionParameters(
  val src: Source,
  val positionalArgs: Seq[JValue],
  val namedArgs: Seq[(String, JValue)],
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
      Await.result(value.members(), duration.Duration.Inf).values.foreach(_.evaluated.await(ctx))
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
      obj.members().flatMap { members =>
        val futures = members.collect {
          case (key, value) if !value.isHidden => value.evaluated.manifestFuture(ctx).map(key -> _)
        }
        Future.sequence(futures).map(members => ManifestedJValue.JObject(members.toMap))
      }
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
      case future: concurrent.Future[EvaluatedJValue.JNow] => EvaluatedJValue.JFuture(Source.Generated, future)
      case future: concurrent.Future[EvaluatedJValue.JFuture] => EvaluatedJValue.JFuture(Source.Generated, future.flatMap(_.future))
      case future: concurrent.Future[EvaluatedJValue] =>
        EvaluatedJValue.JFuture(Source.Generated, future.flatMap {
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
      given ExecutionContext = ctx.executionContext
      obj.imp.lookup(src, field).map(_.evaluated).toJValue

    def members(): Future[collection.Map[String, LazyObjectValue]] =
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
    members.collect {
      case JObjMember.JAssert(src, rawCond, rawMsg) =>
        given ExecutionContext = objCtx.executionContext
        val cond = objCtx.expectBoolean(rawCond)
        val condWithMsg = rawMsg.fold(cond.map(_ -> Option.empty[String])) { msg =>
          cond.zip(objCtx.expectString(msg).map(m => Some(m.str)))
        }
        condWithMsg.foreach { (cond, msgOpt) =>
          if !cond.value then objCtx.error(src, msgOpt.getOrElse("object assertion failed"))
        }
      case _ =>
    }
    obj

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
              key.str -> evalUnsafe(forCtx)(inExpr)
            }
          }
        }).map(_.flatten)
      else
        Future.sequence(arr.elements.map { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          forCtx.expectFieldName(rawKey).map {
            case _: EvaluatedJValue.JNull => None
            case key: EvaluatedJValue.JString => Some(key.str -> evalUnsafe(forCtx)(inExpr))
          }
        }).map(_.flatten)
    }
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
  case JValue.JId(src, name) => ctx.lookup(src, name).evaluated
  case JValue.JGetField(src, loc, field) =>
    given ExecutionContext = ctx.executionContext
    ctx.expectType[EvaluatedJValue.JObject | EvaluatedJValue.JJob | EvaluatedJValue.JPath](loc).flatMap {
      case o: EvaluatedJValue.JObject =>
        o.members().map {
          _
            .getOrElse(field, ctx.error(loc.src, s"object does not have field $field"))
            .evaluated
        }
      case p: EvaluatedJValue.JPath =>
        Future {
          field match
          case "name" => EvaluatedJValue.JString(src, p.path.toString)
          case _ => ctx.error(loc.src, s"path does not have field $field")
        }
      case j: EvaluatedJValue.JJob =>
        Future {
          field match
          case "stdout" => EvaluatedJValue.JString(src, j.stdout)
          case "stderr" => EvaluatedJValue.JString(src, j.stderr)
          case "outputs" => EvaluatedJValue.JArray(src, j.outputs)
          case "exitCode" => EvaluatedJValue.JNum(src, j.exitCode.toDouble)
          case _ => ctx.error(loc.src, s"job does not have field $field")
        }
    }.toJValue
  case JValue.JIndex(src, loc, rawIndex) =>
    given ExecutionContext = ctx.executionContext
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](evalUnsafe(ctx)(loc)).flatMap {
    case obj: EvaluatedJValue.JObject =>
      ctx.expectString(rawIndex).map { field =>
        obj.lookup(src, field.str)
      }
    case arr: EvaluatedJValue.JArray =>
      ctx.expectNum(rawIndex).map(num => arr.elements(num.double.toInt)) // TODO: check index out of bounds
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
      val params = EvaluatedJFunctionParameters(src, positionalArgs, namedArgs)
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
      ctx.expectString(left).zip(ctx.expectObject(right)).flatMap { (left, right) =>
        right.members().map { members =>
          EvaluatedJValue.JBoolean(src, members.contains(left.str))
        }
      }.toJValue

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

object Std:
  private def bindArgs(
    argNames: Seq[(String, Option[EvaluatedJValue])],
    ctx: EvaluationContext,
    params: EvaluatedJFunctionParameters
  ): Array[EvaluatedJValue] =
    val positionalArgs = params.positionalArgs
    val namedArgs = params.namedArgs.toMap
    val numGivenArgs = positionalArgs.size + namedArgs.size
    val result = Array.ofDim[EvaluatedJValue](argNames.size)
    var i = 0
    for
      (argName, defaultOpt) <- argNames
    do
      result(i) =
        if i < positionalArgs.size then
          if namedArgs.contains(argName) then ctx.error(positionalArgs(i).src, s"multiple values provided for argument $argName")
          evalUnsafe(ctx)(positionalArgs(i))
        else if namedArgs.contains(argName) then
          evalUnsafe(ctx)(namedArgs(argName))
        else if defaultOpt.isDefined then
          defaultOpt.get
        else
         ctx.error(params.src, s"no argument provided for $argName")
      i += 1
    result

  type ArgTuple[ArgNames] <: Tuple = ArgNames match
    case EmptyTuple => EmptyTuple
    case (arg *: tail) => EvaluatedJValue *: ArgTuple[tail]

  opaque type Arg[Name] = Option[EvaluatedJValue]

  import scala.language.dynamics
  object Arg extends Dynamic:
    def selectDynamic(name: String): Arg[name.type] = None
    def applyDynamic(name: String)(default: EvaluatedJValue): Arg[name.type] = Some(default)

  extension [T](arg: Arg[T])
    def default: Option[EvaluatedJValue] = arg

  private inline def function0(
    fn: (EvaluationContext, Source) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(Source.Generated, 0, (applyCtx, params) => fn(applyCtx, params.src))

  private inline def function1[Name1 <: String](
    arg1: Arg[Name1],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(Source.Generated, 1, (applyCtx, params) => {
      val Array(a1) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default
      ), applyCtx, params)
      fn(applyCtx, params.src, a1)
    })

  private inline def function2[
    Name1 <: String,
    Name2 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(Source.Generated, 2, (applyCtx, params) => {
      val Array(a1, a2) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
      ), applyCtx, params)
      fn(applyCtx, params.src, a1, a2)
    })

  private inline def function3[
    Name1 <: String,
    Name2 <: String,
    Name3 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
    arg3: Arg[Name3],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction( Source.Generated, 3, (applyCtx, params) => {
      val Array(a1, a2, a3) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
      ), applyCtx, params)
      fn(applyCtx, params.src, a1, a2, a3)
    })

  private inline def function4[
    Name1 <: String,
    Name2 <: String,
    Name3 <: String,
    Name4 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
    arg3: Arg[Name3],
    arg4: Arg[Name4],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction( Source.Generated, 4, (applyCtx, params) => {
      val Array(a1, a2, a3, a4) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
        compiletime.constValue[Name4] -> arg4.default,
      ), applyCtx, params)
      fn(applyCtx, params.src, a1, a2, a3, a4)
    })

  private def makeObject(
    ctx: EvaluationContext,
    staticMembers: Map[String, EvaluatedJValue],
  ): EvaluatedJValue.JObject =
    var objCtx: ObjectEvaluationContext = null
    val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
      Source.Generated,
      new EvaluatedJObject:
        def ctx = objCtx
        def withCtx(newCtx: () => ObjectEvaluationContext) = this
        lazy val cache = {
          given ExecutionContext = ctx.executionContext
          Future(staticMembers.map { (key, value) =>
            key -> LazyValue.strictObject(value, false)
          })
        }
    )
    objCtx = EvaluationContext.ObjectImp(
      ctx.bloopServer,
      obj,
      collection.immutable.Queue.empty,
      ctx,
      Map.empty,
      List.empty,
    )
    obj

  type JFunction1[T] = (EvaluationContext, Source, EvaluatedJValue) => T

  val toStringImp: JFunction1[EvaluatedJValue.JString] = (ctx, src, a) => {
    val value = a match
    case EvaluatedJValue.JString(_, str) => str
    case _ => a.manifest(ctx).toString
    EvaluatedJValue.JString(src, value)
  }

  private val jnull = EvaluatedJValue.JNull(Source.Generated)
  private val jtrue = EvaluatedJValue.JBoolean(Source.Generated, true)
  def obj(ctx: EvaluationContext) = makeObject(ctx, Map(
    "toString" -> function1(Arg.x)(toStringImp),
    "type" -> function1(Arg.x) { (ctx, src, x) =>
      x match
      case x: EvaluatedJValue.JFuture =>
        given concurrent.ExecutionContext = ctx.executionContext
        x.future.map { x =>
          EvaluatedJValue.JString(src, EvaluationContext.typeString(x))
        }.toJValue
      case _ =>
        EvaluatedJValue.JString(src, EvaluationContext.typeString(x))
    },
    "length" -> function1(Arg.x) { (ctx, src, x) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[
        EvaluatedJValue.JArray
        | EvaluatedJValue.JString
        | EvaluatedJValue.JObject
        | EvaluatedJValue.JFunction
      ](x).map {
        case e: EvaluatedJValue.JArray => EvaluatedJValue.JNum(src, e.elements.size)
        case e: EvaluatedJValue.JString => EvaluatedJValue.JNum(src, e.str.size)
        case e: EvaluatedJValue.JObject => e.members().map(m => EvaluatedJValue.JNum(src, m.size)).toJValue
        case e: EvaluatedJValue.JFunction => EvaluatedJValue.JNum(src, e.numParams)
      }.toJValue
    },
    "get" -> function4(Arg.x, Arg.f, Arg.default(jnull), Arg.inc_hidden(jtrue)) {
      (ctx, src, o, f, default, i) =>
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.expectObject(o).zip(ctx.expectString(f)).zip(ctx.expectBoolean(i)).flatMap {
          case ((members, field), inc_hidden) =>
            members.members().map { members =>
              members.get(field.str).fold(default) { m =>
                if !inc_hidden.value && m.isHidden then default else m.evaluated
              }
            }
        }.toJValue
    },
    "objectHas" -> function2(Arg.o, Arg.f) { (ctx, src, o, f) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectObject(o).zip(ctx.expectString(f)).flatMap { (o, f) =>
        o.members().map { members =>
          EvaluatedJValue.JBoolean(src, members.contains(f.str))
        }
      }.toJValue
    },
    "objectFields" -> function1(Arg.o) { (ctx, src, o) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectObject(o).flatMap { o =>
        // EvaluatedJValue.JArray(src, o.members().keys.toSeq.sorted) // BUG
        o.members().map { members =>
          val keys = members
            .keys
            .map(EvaluatedJValue.JString(src, _): EvaluatedJValue.JString)
            .toSeq
            .sortBy(_.str)
          EvaluatedJValue.JArray(src, keys)
        }
      }.toJValue
    },
    "trace" -> function2(Arg.str, Arg.rest) { (ctx, src, str, rest) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectString(str).map { str =>
        val file = ctx.file
        val lineNum = src match
        case Source.Range(start, _) => ":" + file.getLineCol(start)._1
        case _ => ""
        println(s"TRACE: ${file.path}$lineNum: ${str.str}")
        rest
      }.toJValue
    },
    "workspace" -> function0 { (ctx, src) =>
      EvaluatedJValue.JString(src, ctx.workspaceDir.toString)
    },
    "runJob" -> function1(Arg.desc) { (ctx, src, desc) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.decode[JobDescription](desc).flatMap(ctx.runJob(src, _)).toJValue
    },
    "source" -> function1(Arg.pathName) { (ctx, src, pathName) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[EvaluatedJValue.JString](pathName).map { pathName =>
        import JobRunner.resolvePath
        if pathName.str.startsWith("/") then ctx.error(src, s"cannot source an absolute path, got $pathName")
        EvaluatedJValue.JPath(src, ctx.resolvePath(pathName.str))
      }.toJValue
    },
    "write" -> function2(Arg.pathName, Arg.contents) { (ctx, src, pathName, contents) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[EvaluatedJValue.JString | EvaluatedJValue.JPath](pathName).flatMap { pathName =>
        ctx.expectString(contents).map { contents =>
          val path =
            pathName match
            case str: EvaluatedJValue.JString =>
              if str.str.startsWith("/") then
                java.nio.file.Paths.get(str.str)
              else
                ctx.workspaceDir.resolve(str.str)
            case path: EvaluatedJValue.JPath => path.path
          EvaluatedJValue.JPath(src, JobRunner.write(path, contents.str))
        }
      }.toJValue
    },
    "getenv" -> function1(Arg.varName) { (ctx, src, varName) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[EvaluatedJValue.JString](varName).map { varNamex =>
        val varName = varNamex.str
        try
          val value = System.getenv(varName)
          if value eq null then
            ctx.error(src, s"environment variable \"$varName\" not set")
          else
            EvaluatedJValue.JString(src, value)
        catch
          case e: java.lang.SecurityException => ctx.error(src, s"could not access environment variable \"$varName\": ${e.getMessage}")
      }.toJValue
    },
    "scala" -> makeObject(ctx, Map(
      "cs" -> function1(Arg.deps) { (ctx, src, deps) =>
        import coursier.{Dependency, Fetch, Module, ModuleName, Organization}
        import coursier.cache.FileCache
        import coursier.cache.loggers.RefreshLogger
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.decode[Seq[CoursierDependency]](deps).flatMap { deps =>
          Fetch()
            .withDependencies(deps.map(_.toDependency))
            // .addDependencies(params.deps.map(_.toDependency)) // BUG
            .withCache(
              FileCache().withLogger(RefreshLogger.create(System.out))
            )
            .future()
            .map { files =>
              EvaluatedJValue.JArray(src, files.map(a => EvaluatedJValue.JPath(src, a.toPath)))
            }
        }.toJValue
      },
      "compile" -> function2(Arg.targetId, Arg.configPaths) { (ctx, src, targetId, configPaths) =>
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.expectString(targetId).flatMap { targetId =>
          ctx.decode[Seq[EvaluatedJValue.JPath]](configPaths).flatMap { _ =>
            ctx.compile(src, targetId.str)
          }
        }.toJValue
      },
      "classpath" -> function2(Arg.targetId, Arg.configPaths) { (ctx, src, targetId, configPaths) =>
        given concurrent.ExecutionContext = ctx.executionContext
        import scala.jdk.CollectionConverters.given
        import ch.epfl.scala.bsp4j.StatusCode
        ctx.expectString(targetId).flatMap { targetId =>
          ctx.decode[Seq[EvaluatedJValue.JPath]](configPaths).flatMap { paths =>
            ctx.bloopServer.jvmRunEnvironment(targetId.str).map {
              case Right(env) =>
                val strings = env.getItems.get(0).getClasspath.asScala.map { item =>
                  val file = java.nio.file.Paths.get(new java.net.URI(item))
                  EvaluatedJValue.JPath(src, file)
                }.toSeq
                EvaluatedJValue.JArray(src, strings)
              case Left(StatusCode.ERROR) => ctx.error(src, s"compilation for $targetId failed")
              case Left(StatusCode.CANCELLED) => ctx.error(src, s"compilation for $targetId was cancelled")
            }
          }
        }.toJValue
      },
    ))
  ))
