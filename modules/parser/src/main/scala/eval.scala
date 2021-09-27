package root

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
    arrayElementsFuture: concurrent.Future[Seq[(String, EvaluatedJValue)]],
    forVar: String,
    value: JValue,
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()
    def withCtx(newCtx: () => ObjectEvaluationContext) =
      comprehension(newCtx, arrayElementsFuture, forVar, value)

    lazy val cache = {
      implicit val ec = ctx.executionContext
      val future = arrayElementsFuture.map(_.map { (key, e) =>
        val valueCtx = ctx.bindEvaluated(forVar, e).withStackEntry(StackEntry.objectField(ctx.file, value.src))
        key -> LazyValue(valueCtx, value, false)
      }.toMap)
      concurrent.Await.result(future, concurrent.duration.Duration.Inf)
    }

  def apply(
    ctxThunk: () => ObjectEvaluationContext,
    rawMembers: Seq[JObjMember.JField],
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()

    def withCtx(newCtx: () => ObjectEvaluationContext) =
      apply(newCtx, rawMembers)

    lazy val cache = {
      implicit val ec = ctx.executionContext
      val result = collection.mutable.HashMap[String, LazyObjectValue]()
      val futures = rawMembers.map { case JObjMember.JField(src, rawKey, plus, isHidden, value) =>
        ctx.expectFieldName(rawKey).map {
          case _: EvaluatedJValue.JNull => None
          case expr: EvaluatedJValue.JString =>
            val key = expr.str
            val valueCtx = ctx.withStackEntry(StackEntry.objectField(ctx.file, value.src))
            Some(
              if plus then
                key -> LazyValue(
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
                key -> LazyValue(valueCtx, value, isHidden)
          )
        }
      }
      concurrent.Await.result(concurrent.Future.sequence(futures).map(_.flatten.toMap), concurrent.duration.Duration.Inf)
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
  case JFunction(src: Source, numParams: Int, fn: (EvaluationContext, EvaluatedJFunctionParameters) => EvaluatedJValue)
  case JFuture(src: Source, future: concurrent.Future[EvaluatedJValue.JNow])

  def isNull: Boolean =
    this match
    case _: JNull => true
    case _ => false

  import concurrent.{Await, Future, ExecutionContext, duration}
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
      val members = obj.members().collect {
        case (key, value) if !value.isHidden => value.evaluated.manifestFuture(ctx).map(key -> _)
      }
      Future.sequence(members).map(members => ManifestedJValue.JObject(members.toMap))
    case fn: JFunction => ctx.error(fn.src, "cannot manifest function")
    case f: JFuture => f.future.map(_.manifest(ctx))

  def manifest(ctx: EvaluationContext): ManifestedJValue =
      Await.result(manifestFuture(ctx), duration.Duration.Inf)

object EvaluatedJValue:
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
    | JArray
    | JObject
    | JFunction
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
        implicit val ec = objCtx.executionContext
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
    var objCtx: ObjectEvaluationContext = null
    implicit val ec = ctx.executionContext
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
    implicit val ec = ctx.executionContext
    ctx
      .expectObject(loc)
      .map(
        _
          .members()
          .getOrElse(field, ctx.error(loc.src, s"object does not have field $field"))
          .evaluated
      )
      .toJValue
  case JValue.JIndex(src, loc, rawIndex) =>
    implicit val ec = ctx.executionContext
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
          resultCtx = op2.ctx.withSelf(result).withParent(parent)
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
  case JValue.JImport(src, file) =>
    ctx.importFile(src, file)
  case JValue.JImportStr(src, file) =>
    ctx.importStr(src, file)
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
  private val ctx = new EvaluationContext.Imp(
    SourceFile.std,
    Map.empty,
    List.empty,
    concurrent.ExecutionContext.global,
  )

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

  private inline def function1[Name1 <: String](
    arg1: Arg[Name1],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction( Source.Generated, 1, (applyCtx, params) => {
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
    staticMembers: Map[String, EvaluatedJValue],
  ): EvaluatedJValue.JObject =
    var objCtx: ObjectEvaluationContext = null
    val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
      Source.Generated,
      new EvaluatedJObject:
        def ctx = objCtx
        def withCtx(newCtx: () => ObjectEvaluationContext) = this
        val cache = staticMembers.map { (key, value) =>
          key -> LazyValue.strictObject(value, false)
        }
    )
    objCtx = EvaluationContext.ObjectImp(
      obj,
      None,
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
  val obj = makeObject(Map(
    "toString" -> function1(Arg.x)(toStringImp),
    "type" -> function1(Arg.x) { (ctx, src, x) =>
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
        case e: EvaluatedJValue.JArray => e.elements.size
        case e: EvaluatedJValue.JString => e.str.size
        case e: EvaluatedJValue.JObject => e.members().size
        case e: EvaluatedJValue.JFunction => e.numParams
      }.map {
        EvaluatedJValue.JNum(src, _)
      }.toJValue
    },
    "get" -> function4(Arg.x, Arg.f, Arg.default(jnull), Arg.inc_hidden(jtrue)) {
      (ctx, src, o, f, default, i) =>
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.expectObject(o).zip(ctx.expectString(f)).zip(ctx.expectBoolean(i)).map {
          case ((members, field), inc_hidden) =>
            members.members().get(field.str).fold(default) { m =>
              if !inc_hidden.value && m.isHidden then default else m.evaluated
            }
        }.toJValue
    },
    "objectHas" -> function2(Arg.o, Arg.f) { (ctx, src, o, f) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectObject(o).zip(ctx.expectString(f)).map { (o, f) =>
        EvaluatedJValue.JBoolean(src, o.members().contains(f.str))
      }.toJValue
    },
    "objectFields" -> function1(Arg.o) { (ctx, src, o) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectObject(o).map { o =>
        // EvaluatedJValue.JArray(src, o.members().keys.toSeq.sorted) // BUG
        val keys =
          o
            .members()
            .keys
            .map(EvaluatedJValue.JString(src, _): EvaluatedJValue.JString)
            .toSeq
            .sortBy(_.str)
        EvaluatedJValue.JArray(src, keys)
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
    "scala" -> makeObject(Map(
      "cs" -> function1(Arg.deps) { (ctx, src, deps) =>
        import coursier.{Dependency, Fetch, Module, ModuleName, Organization}
        given concurrent.ExecutionContext = ctx.executionContext
        JDecoder[Seq[CoursierDependency]].decode(ctx, deps).flatMap { deps =>
          Fetch()
            .withDependencies(deps.map(_.toDependency))
            // .addDependencies(params.deps.map(_.toDependency)) // BUG
            .future()
            .map { files =>
              EvaluatedJValue.JArray(src, files.map(a => EvaluatedJValue.JString(src, a.toString)))
            }
        }.toJValue
      },
    ))
  ))
