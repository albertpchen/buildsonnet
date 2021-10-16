package root

import monix.eval.Task
import concurrent.{ExecutionContext, Future}
import cats.syntax.all._
import cats.data.Nested

sealed trait EvaluatedJObject:
  def ctx: ObjectEvaluationContext
  protected def cache: collection.Map[String, LazyObjectValue]

  def withCtx(newCtx: () => ObjectEvaluationContext): EvaluatedJObject

  def lookup(src: Source, field: String): LazyValue =
    cache.getOrElse(field, LazyValue.strict(ctx.error(src, s"object missing field $field")))

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
  type Underlying

  def isNull: Boolean =
    this match
    case _: JNull => true
    case _ => false

  def manifestFuture(ctx: EvaluationContext): Task[Either[EvaluatedJValue.JError, ManifestedJValue]] =
    this match
    case value: JBoolean => Task.pure(Right(ManifestedJValue.JBoolean(value.value)))
    case value: JNull => Task.pure(Right(ManifestedJValue.JNull))
    case value: JString => Task.pure(Right(ManifestedJValue.JString(value.str)))
    case value: JNum => Task.pure(Right(ManifestedJValue.JNum(value.double)))
    case value: JArray =>
      val futures = value.elements.map(_.manifestFuture(ctx))
      Task.parSequence(futures).map(_.foldLeft(
        Right(Seq.empty[ManifestedJValue]): Either[EvaluatedJValue.JError, Seq[ManifestedJValue]]
      ) { (acc, e) =>
        acc.flatMap(acc => e.map(_ +: acc))
      }.map(elements => ManifestedJValue.JArray(elements.reverse)))
    case obj: JObject =>
      val members = obj.members()
      val tasks = members.collect {
        case (key, value) if !value.isHidden =>
          value.evaluated.flatMap(_.manifestFuture(ctx).map(_.map(key -> _)))
      }
      Task.parSequence(tasks).map(_.foldLeft(
        Right(Seq.empty[(String, ManifestedJValue)]): Either[EvaluatedJValue.JError, Seq[(String, ManifestedJValue)]]
      ) { (acc, pair) =>
        acc.flatMap(acc => pair.map(_ +: acc))
      }.map(members => ManifestedJValue.JObject(members.toMap)))
    case expr => Task.pure(Left(ctx.error(expr.src, s"cannot manifest ${EvaluationContext.typeString(expr)}")))

  def manifest(ctx: EvaluationContext): Either[EvaluatedJValue.JError, ManifestedJValue] =
    given monix.execution.Scheduler = ???
    Await.result(manifestFuture(ctx).runToFuture, duration.Duration.Inf)

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
  case class JFunction(src: Source, numParams: Int, fn: (EvaluationContext, EvaluatedJFunctionParameters) => Task[EvaluatedJValue]) extends EvaluatedJValue
  case class JError(src: Source, message: String, stack: List[StackEntry]) extends EvaluatedJValue

  extension (obj: EvaluatedJValue.JObject)
    def ctx: ObjectEvaluationContext = obj.imp.ctx

    def withCtx(ctx: () => ObjectEvaluationContext): EvaluatedJValue.JObject =
      obj.copy(imp = obj.imp.withCtx(ctx))

    def lookup(src: Source, field: String): Task[EvaluatedJValue] =
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
    def structuralEquals(other: EvaluatedJValue)(using ExecutionContext): Task[Boolean] =
      (value, other) match
      case (op1: JBoolean, op2: JBoolean) => Task.now(op1.value == op2.value)
      case (op1: JNull, op2: JNull) => Task.now(true)
      case (op1: JString, op2: JString) => Task.now(op1.str == op2.str)
      case (op1: JNum, op2: JNum) => Task.now(op1.double == op2.double)
      case (op1: JPath, op2: JPath) => Task.now(op1.path == op2.path)
      case (op1: JJob, op2: JJob) => Task.now(op1 eq op2)
      case (op1: JArray, op2: JArray) =>
        if op1.elements.size == op2.elements.size then
          op1.elements.zip(op2.elements).foldLeft(Task.now(true)) {
            case (result, (op1, op2)) => result.flatMap { result =>
              if result then
                op1.structuralEquals(op2)
              else
                Task.now(result)
            }
          }
        else
          Task.now(false)
      case (op1: JFunction, op2: JFunction) => Task.now(op1 eq op2)
      case (op1: JObject, op2: JObject) =>
        val members1 = op1.members()
        val members2 = op2.members()
        if members1.keys == members2.keys then
          members1.keys.foldLeft(Task.now(true)) {
            case (result, key) => result.flatMap { result =>
              if result then
                members1(key).evaluated.zip(members2(key).evaluated).flatMap(_.structuralEquals(_))
              else
                Task.now(result)
            }
          }
        else Task.now(false)
      case (op1, op2) => Task.now(false)

def manifest(ctx: EvaluationContext)(jvalue: JValue): Task[Either[EvaluatedJValue.JError, ManifestedJValue]] =
  val evaluated = evalUnsafe(ctx)(jvalue)
  evaluated.map(_.manifest(ctx))

private def applyArgs(
  defCtx: EvaluationContext,
  fnSrc: Source,
  paramsDef: JParamList,
  body: JValue,
)(applyCtx: EvaluationContext, params: EvaluatedJFunctionParameters): Task[EvaluatedJValue] =
  val positionalArgs = params.positionalArgs
  val namedArgs = params.namedArgs
  val numGivenArgs = positionalArgs.size + namedArgs.size
  if numGivenArgs > paramsDef.size then
    applyCtx.error(params.src, "to many arguments for function")
  val argMap = namedArgs.toMap
  paramsDef.foldLeft[Either[EvaluatedJValue.JError, (Seq[EvaluatedJValue], EvaluationContext)]](
    Right(positionalArgs -> defCtx)
  ) { (either, nameDefault) =>
    either.flatMap { (positionalArgs, ctx) =>
      val (argName, default) = nameDefault
      val isGivenNamedArg = argMap.contains(argName)
      if positionalArgs.nonEmpty && isGivenNamedArg then
        Left(applyCtx.error(params.src, s"both positional and named arg provided for argument $argName"))
      else if positionalArgs.nonEmpty then
        Right((positionalArgs.tail, ctx.bindEvaluated(argName, positionalArgs.head)))
      else if isGivenNamedArg then
        Right((positionalArgs, ctx.bindEvaluated(argName, argMap(argName))))
      else if default.isDefined then
        Right((positionalArgs, ctx.bindWithCtx(argName, defCtx, default.get)))
      else
        Left(applyCtx.error(params.src, s"missing argument $argName"))
    }
  }.fold(
    Task.pure(_),
    (_, argsCtx) => {
      val functionCtx = argsCtx.functionCtx(fnSrc)
      evalUnsafe(functionCtx)(body)
    }
  )

def evalUnsafe(ctx: EvaluationContext)(jvalue: JValue): Task[EvaluatedJValue] =
  import concurrent.Future
  jvalue match
  case JValue.JFalse(src) => Task.pure(EvaluatedJValue.JBoolean(src, false))
  case JValue.JTrue(src) => Task.pure(EvaluatedJValue.JBoolean(src, true))
  case JValue.JNull(src) => Task.pure(EvaluatedJValue.JNull(src))
  case JValue.JSelf(src) => Task.pure(ctx.self(src))
  case JValue.JSuper(src) => Task.pure(ctx.`super`(src))
  case JValue.JOuter(src) => ctx.lookup(src, "$").evaluated
  case JValue.JString(src, str) => Task.pure(EvaluatedJValue.JString(src, str))
  case JValue.JNum(src, str) => Task.pure(EvaluatedJValue.JNum(src, str.toDouble))
  case JValue.JArray(src, elements) =>
    Task.parSequence(elements.map(evalUnsafe(ctx))).map(EvaluatedJValue.JArray(src, _))
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
      }.value
    }
    Task.parSequence(members).flatMap { members =>
      val typedMembers = members.sequence.map { members =>
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
              (cond, objCtx.expectString(msg)).mapN((a, b) => a -> Some(b))
            }
            condWithMsg.map { (cond, msgOpt) =>
              if !cond then objCtx.error(src, msgOpt.getOrElse("object assertion failed"))
            }.value
        }
        Task.parSequence(asserts).map(_ => obj)
      }
      typedMembers.fold(
        e => Task(e),
        identity,
      )
    }

  case JValue.JObjectComprehension(src, preLocals, rawKey, value, postLocals, forVar, inExpr, condOpt) =>
    given ExecutionContext = ctx.executionContext
    ctx.expectArray(inExpr).value.flatMap { arr =>
      arr.fold(
        Task.now(_),
        arr => {
          val arrElements = if condOpt.isDefined then
            val cond = condOpt.get
            val tasks: Task[Seq[TypedJValue[Option[(String, EvaluatedJValue)]]]] = Task.parSequence(arr.map { e =>
              val forCtx = ctx.bindEvaluated(forVar, e)
              (forCtx.expectFieldName(rawKey), ctx.expectBoolean(cond)).mapN {
                case (_: EvaluatedJValue.JNull, _) => None
                case (key: EvaluatedJValue.JString, cond) => Option.when(cond) {
                  key.str -> e
                }
              }.value
            })
            tasks.map(_.sequence.map(_.flatten))
          else
            Task.parSequence(arr.map { e =>
              val forCtx = ctx.bindEvaluated(forVar, e)
              forCtx.expectFieldName(rawKey).map {
                case _: EvaluatedJValue.JNull => None
                case key: EvaluatedJValue.JString => Some(key.str -> e)
              }.value
            }).map(_.sequence.map(_.flatten))
          Nested[Task, TypedJValue, Seq[(String, EvaluatedJValue)]](arrElements).map { arrElements =>
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
          }.value.map(_.toEvaluated)
        }
      )
    }
  case JValue.JId(src, name) => ctx.lookup(src, name).evaluated
  case JValue.JGetField(src, loc, field) =>
    ctx
      .expectType[EvaluatedJValue.JObject | EvaluatedJValue.JJob | EvaluatedJValue.JPath](loc)
      .value
      .flatMap(_.fold(Task.now, {
        case o: EvaluatedJValue.JObject =>
          o
            .members()
            .get(field)
            .fold(Task.now(ctx.error(loc.src, s"object does not have field $field")))(_.evaluated)
        case p: EvaluatedJValue.JPath => Task.now {
          field match
          case "name" => EvaluatedJValue.JString(src, p.path.toString)
          case _ => ctx.error(loc.src, s"path does not have field $field")
        }
        case j: EvaluatedJValue.JJob => Task.now {
          field match
          case "stdout" => EvaluatedJValue.JString(src, j.stdout)
          case "stderr" => EvaluatedJValue.JString(src, j.stderr)
          case "outputs" => EvaluatedJValue.JArray(src, j.outputs)
          case "exitCode" => EvaluatedJValue.JNum(src, j.exitCode.toDouble)
          case _ => ctx.error(loc.src, s"job does not have field $field")
        }
      }))
  case JValue.JIndex(src, loc, rawIndex) =>
    given ExecutionContext = ctx.executionContext
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](loc).value.flatMap {
      _.fold(Task.now, {
        case obj: EvaluatedJValue.JObject =>
          ctx.expectString(rawIndex).value.flatMap {
            _.fold(Task.now, obj.lookup(src, _))
          }
        case arr: EvaluatedJValue.JArray =>
          ctx.expectNum(rawIndex).value.map {
            _.fold(identity, num => {
              val idx = num.toInt
              if idx >= arr.elements.size then
                ctx.error(src, s"index $idx out of bounds for length ${arr.elements.size}")
              arr.elements(idx)
            })
          }
      })
    }
  case JValue.JSlice(src, loc, rawIndex, rawEndIndex, rawStride) =>
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](loc).value.flatMap {
      _.fold(Task.now, {
        case obj: EvaluatedJValue.JObject =>
          Task.now(ctx.error(src, "no end index or stride allowed for object index"))
        case arr: EvaluatedJValue.JArray =>
          val index = ctx.expectNum(rawIndex)
          val none = Nested[Task, TypedJValue, Option[Int]](Task.now(TypedJValue(Option.empty[Int])))
          val endIndex = rawEndIndex.fold(none)(num => ctx.expectNum(num).map(n => Some(n.toInt)))
          val stride = rawStride.fold(none)(num => ctx.expectNum(num).map(n => Some(n.toInt)))
          (index, endIndex, stride).mapN { case (index, endIndex, stride) =>
            arr.index(src, ctx, index.toInt, endIndex, stride)
          }.value.map(_.toEvaluated)
      })
    }
  case JValue.JApply(src, loc, positionalArgs, namedArgs) =>
    ctx.expectFunction(loc).value.flatMap {
      _.fold(Task.now, fn => {
        for
          positionalArgs <- Task.parSequence(positionalArgs.map(evalUnsafe(ctx)))
          namedArgs <- Task.parSequence(namedArgs.map((n, a) => evalUnsafe(ctx)(a).map(n -> _)))
          params = EvaluatedJFunctionParameters(
            src,
            positionalArgs,
            namedArgs,
          )
          result <- fn.fn(ctx, params)
        yield
          result
      })
    }
  case JValue.JBinaryOp(src, leftJValue, op, rightJValue) =>
    given concurrent.ExecutionContext = ctx.executionContext
    op match
    case JBinaryOperator.Op_+ =>
      type PlusOperand = String | Double | EvaluatedJObject | Seq[EvaluatedJValue]
      (ctx.expectType[PlusOperand](leftJValue), ctx.expectType[PlusOperand](rightJValue)).mapN {
        case (op1: String, op2) =>
          EvaluatedJValue.JString(src, op1 + Std.toStringImp(ctx, rightJValue.src, op2).str)
        case (op1, op2: String) =>
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
      }
    case JBinaryOperator.Op_- =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JNum(src, left - right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_* =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JNum(src, left * right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_/ =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JNum(src, left / right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_< =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JBoolean(src, left < right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_<= =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JBoolean(src, left <= right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_> =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JBoolean(src, left > right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_>= =>
      (ctx.expectNum(leftJValue), ctx.expectNum(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JBoolean(src, left >= right)
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_>> =>
      (ctx.expectNum(rightJValue), ctx.expectNum(leftJValue)).mapN { (right, left) =>
        val rhs = right.toLong
        if rhs >= 0 then
          val shamt = rhs % 64
          EvaluatedJValue.JNum(src, (left.toLong >> shamt).toDouble)
        else
          ctx.error(rightJValue.src, s"shift amount cannot be negative, got $rhs")
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_<< =>
      (ctx.expectNum(rightJValue), ctx.expectNum(leftJValue)).mapN { (right, left) =>
        val rhs = right.toLong
        if rhs >= 0 then
          val shamt = rhs % 64
          EvaluatedJValue.JNum(src, (left.toLong << shamt).toDouble)
        else
          ctx.error(rightJValue.src, s"shift amount cannot be negative, got $rhs")
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_in =>
      (ctx.expectString(leftJValue), ctx.expectObject(rightJValue)).mapN { (left, right) =>
        EvaluatedJValue.JBoolean(src, right.members().contains(left))
      }.value.map(_.toEvaluated)
    case JBinaryOperator.Op_== =>
      (evalUnsafe(ctx)(leftJValue), evalUnsafe(ctx)(rightJValue))
        .parMapN((a, b) => a.structuralEquals(b).map(EvaluatedJValue.JBoolean(src, _)))
        .flatten
    case JBinaryOperator.Op_!= =>
      (evalUnsafe(ctx)(leftJValue), evalUnsafe(ctx)(rightJValue))
        .parMapN((a, b) => a.structuralEquals(b).map(b => EvaluatedJValue.JBoolean(src, !b)))
        .flatten

  case JValue.JUnaryOp(src, op, rawOperand) =>
    op match
    case JUnaryOperator.Op_! =>
      ctx.expectBoolean(rawOperand).map { operand =>
        EvaluatedJValue.JBoolean(src, !operand)
      }.value.map(_.toEvaluated)
    case JUnaryOperator.Op_+ =>
      ctx
        .expectNum(rawOperand)
        .map(EvaluatedJValue.JNum(rawOperand.src, _))
        .value
        .map(_.toEvaluated)
    case JUnaryOperator.Op_- =>
      ctx
        .expectNum(rawOperand)
        .map(operand => EvaluatedJValue.JNum(src, -operand))
        .value
        .map(_.toEvaluated)
    case JUnaryOperator.Op_~  =>
      ctx
        .expectNum(rawOperand)
        .map(operand => EvaluatedJValue.JNum(src, (~operand.toLong).toDouble))
        .value
        .map(_.toEvaluated)
  case JValue.JLocal(_, name, value, result) =>
    evalUnsafe(ctx.bind(name, value))(result)
  case JValue.JFunction(src, params, body) =>
    Task.now(EvaluatedJValue.JFunction(src, params.size, applyArgs(ctx, src, params, body)))
  case JValue.JIf(src, rawCond, trueValue, elseValue) =>
    ctx.expectBoolean(rawCond).value.flatMap {
      _.fold(Task.now, cond => {
        if cond then
          evalUnsafe(ctx)(trueValue)
        else
          elseValue.fold(Task.now(EvaluatedJValue.JNull(src)))(evalUnsafe(ctx))
      })
    }
  case JValue.JError(src, rawExpr) =>
    ctx
      .expectString(rawExpr)
      .map(ctx.error(src, _))
      .value
      .map(_.toEvaluated)
  case JValue.JAssert(src, rawCond, rawMsg, expr) =>
    val msg = rawMsg.fold(Nested[Task, TypedJValue, Option[String]](Task.now(TypedJValue(Option.empty[String])))) { msg => ctx.expectString(msg).map(Some(_)) }
    (ctx.expectBoolean(rawCond), msg).mapN { (cond, msgOpt) =>
      if !cond then
        Task.now(ctx.error(src, msgOpt.getOrElse(s"assertion failed")))
      else
        evalUnsafe(ctx)(expr)
    }.value.flatMap(_.fold(Task.now, identity))
  case JValue.JImport(src, file) => ctx.`import`(src, file)
  case JValue.JImportStr(src, file) => ctx.importStr(src, file)
  case JValue.JArrayComprehension(src, forVar, forExpr, inExpr, condOpt) =>
    val cond = condOpt.fold(Nested[Task, TypedJValue, Boolean](Task.now(TypedJValue(true)))) { cond => ctx.expectBoolean(cond) }
    ctx
      .expectArray(inExpr)
      .value
      .flatMap {
        _.fold(
          Task.now,
          array => if condOpt.isDefined then
            Task.parSequence(array.map { e =>
              val forCtx = ctx.bindEvaluated(forVar, e)
              forCtx.expectBoolean(condOpt.get).map { cond =>
                Option.when(cond)(evalUnsafe(forCtx)(forExpr))
              }.value
            }).flatMap { array =>
              array.sequence.fold(
                Task.now,
                array => Task.parSequence(array.flatten).map(EvaluatedJValue.JArray(src, _))
              )
            }
          else
            Task.parSequence(array.map { e =>
              val forCtx = ctx.bindEvaluated(forVar, e)
              evalUnsafe(forCtx)(forExpr)
            }).map(EvaluatedJValue.JArray(src, _))
        )
      }
