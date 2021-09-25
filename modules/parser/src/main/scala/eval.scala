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
    arrayElements: Seq[(String, EvaluatedJValue)],
    forVar: String,
    value: JValue,
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()
    def withCtx(newCtx: () => ObjectEvaluationContext) =
      comprehension(newCtx, arrayElements, forVar, value)

    lazy val cache = {
      val result = collection.mutable.HashMap[String, LazyObjectValue]()
      arrayElements.foreach { (key, e) =>
        val valueCtx = ctx.bindEvaluated(forVar, e).withStackEntry(StackEntry.objectField(ctx.file, value.src))
        result(key) = LazyValue(valueCtx, value, false)
      }
      result
    }

  def apply(
    ctxThunk: () => ObjectEvaluationContext,
    rawMembers: Seq[JObjMember.JField],
  ): EvaluatedJObject = new EvaluatedJObject:
    def ctx = ctxThunk()

    def withCtx(newCtx: () => ObjectEvaluationContext) =
      apply(newCtx, rawMembers)

    lazy val cache = {
      val result = collection.mutable.HashMap[String, LazyObjectValue]()
      rawMembers.foreach { case JObjMember.JField(src, rawKey, plus, isHidden, value) =>
        val key = ctx.expectFieldName(rawKey).str
        val valueCtx = ctx.withStackEntry(StackEntry.objectField(ctx.file, value.src))
        if plus then
          result(key) = LazyValue(
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
          result(key) = LazyValue(valueCtx, value, isHidden)
      }
      result
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

  def manifest(ctx: EvaluationContext): ManifestedJValue =
    this match
    case value: JBoolean => ManifestedJValue.JBoolean(value.value)
    case value: JNull => ManifestedJValue.JNull
    case value: JString => ManifestedJValue.JString(value.str)
    case value: JNum => ManifestedJValue.JNum(value.double)
    case value: JArray =>
      val elements = value.elements.foldLeft(Seq.empty[ManifestedJValue]) {
        (tail, element) => element.manifest(ctx) +: tail
      }.reverse.toVector
      ManifestedJValue.JArray(elements)
    case obj: JObject =>
      val members = obj.members().toSeq.sortBy(_._1).foldLeft(Seq.empty[(String, ManifestedJValue)]) {
        case (tail, (key, value)) => (key -> value.evaluated.manifest(ctx)) +: tail
      }.reverse.toMap
      ManifestedJValue.JObject(members)
    case fn: JFunction => ctx.error(fn.src, "cannot manifest function")

object EvaluatedJValue:
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
    members.foreach {
      case JObjMember.JAssert(src, rawCond, rawMsg) =>
        val cond = objCtx.expectBoolean(rawCond).value
        val msgOpt = rawMsg.map(msg => objCtx.expectString(msg).str)
        if !cond then objCtx.error(src, msgOpt.getOrElse("object assertion failed"))
      case _ =>
    }
    obj

  case JValue.JObjectComprehension(src, preLocals, rawKey, value, postLocals, forVar, inExpr, condOpt) =>
    var objCtx: ObjectEvaluationContext = null
    val arr = ctx.expectArray(inExpr).elements
    val arrElements =
      if condOpt.isDefined then
        val cond = condOpt.get
        arr.flatMap { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          val key = forCtx.expectFieldName(rawKey).str
          Option.when(ctx.expectBoolean(cond).value) {
            key -> evalUnsafe(forCtx)(inExpr)
          }
        }
      else
        arr.map { e =>
          val forCtx = ctx.bindEvaluated(forVar, e)
          val key = forCtx.expectFieldName(rawKey).str
          key -> evalUnsafe(forCtx)(inExpr)
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
    ctx
      .expectObject(loc)
      .members()
      .getOrElse(field, ctx.error(loc.src, s"object does not have field $field"))
      .evaluated
  case JValue.JIndex(src, loc, rawIndex) =>
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](evalUnsafe(ctx)(loc)) match
    case obj: EvaluatedJValue.JObject =>
      val field = ctx.expectString(rawIndex)
      obj.lookup(src, field.str)
    case arr: EvaluatedJValue.JArray =>
      val index = ctx.expectNum(rawIndex).double.toInt
      arr.elements(index)
  case JValue.JSlice(src, loc, rawIndex, rawEndIndex, rawStride) =>
    // println(s"$rawIndex, $rawEndIndex, $rawStride")
    ctx.expectType[EvaluatedJValue.JArray | EvaluatedJValue.JObject](loc) match
    case obj: EvaluatedJValue.JObject =>
      ctx.error(src, "no end index or stride allowed for object index")
    case arr: EvaluatedJValue.JArray =>
      val index = ctx.expectNum(rawIndex).double.toInt
      val endIndex = rawEndIndex.map(ctx.expectNum(_).double.toInt)
      val stride = rawStride.map(ctx.expectNum(_).double.toInt)
      arr.index(src, ctx, index, endIndex, stride)
  case JValue.JApply(src, loc, positionalArgs, namedArgs) =>
    val fn = ctx.expectFunction(loc)
    val params = EvaluatedJFunctionParameters(src, positionalArgs, namedArgs)
    fn.fn(ctx, params)
  case JValue.JBinaryOp(src, left, op, right) =>
    op match
    case JBinaryOperator.Op_+ =>
      (evalUnsafe(ctx)(left), evalUnsafe(ctx)(right)) match
        case (op1: EvaluatedJValue.JString, op2) =>
          EvaluatedJValue.JString(src, op1.str + Std.toStringImp(ctx, op2.src, op2).str)
        case (op1, op2: EvaluatedJValue.JString) =>
          EvaluatedJValue.JString(src, Std.toStringImp(ctx, op1.src, op1).str + op2)
        case (left, right) =>
          ctx.expectType[EvaluatedJValue.JNum | EvaluatedJValue.JObject | EvaluatedJValue.JArray | EvaluatedJValue.JString](left) match
            case op1: EvaluatedJValue.JNum =>
              val op2 = ctx.expectNum(right)
              EvaluatedJValue.JNum(src, op1.double + op2.double)
            case op1: EvaluatedJValue.JObject =>
              val op2 = ctx.expectObject(right)
              var parentCtx: ObjectEvaluationContext = null
              val parent: EvaluatedJValue.JObject = op1.withCtx(ctx = () => parentCtx)
              var resultCtx: ObjectEvaluationContext = null
              val result: EvaluatedJValue.JObject = op2.withCtx(ctx = () => resultCtx)
              resultCtx = op2.ctx.withSelf(result).withParent(parent)
              parentCtx = op1.ctx.withSelf(result)
              result
            case op1: EvaluatedJValue.JArray =>
              val op2 = ctx.expectArray(right)
              EvaluatedJValue.JArray(src, op1.elements ++ op2.elements)
    case JBinaryOperator.Op_- =>
      EvaluatedJValue.JNum(src, ctx.expectNum(left).double - ctx.expectNum(right).double)
    case JBinaryOperator.Op_* =>
      EvaluatedJValue.JNum(src, ctx.expectNum(left).double * ctx.expectNum(right).double)
    case JBinaryOperator.Op_/ =>
      EvaluatedJValue.JNum(src, ctx.expectNum(left).double / ctx.expectNum(right).double)
    case JBinaryOperator.Op_< =>
      EvaluatedJValue.JBoolean(src, ctx.expectNum(left).double < ctx.expectNum(right).double)
    case JBinaryOperator.Op_<= =>
      EvaluatedJValue.JBoolean(src, ctx.expectNum(left).double <= ctx.expectNum(right).double)
    case JBinaryOperator.Op_> =>
      EvaluatedJValue.JBoolean(src, ctx.expectNum(left).double > ctx.expectNum(right).double)
    case JBinaryOperator.Op_>= =>
      EvaluatedJValue.JBoolean(src, ctx.expectNum(left).double >= ctx.expectNum(right).double)
    case JBinaryOperator.Op_>> =>
      val rhs = ctx.expectNum(right).double.toLong
      val shamt =
        if rhs >= 0 then
          rhs % 64
        else
          ctx.error(right.src, s"shift amount cannot be negative, got $rhs")
      EvaluatedJValue.JNum(src, (ctx.expectNum(left).double.toLong >> shamt).toDouble)
    case JBinaryOperator.Op_<< =>
      val rhs = ctx.expectNum(right).double.toLong
      val shamt =
        if rhs >= 0 then
          rhs % 64
        else
          ctx.error(right.src, s"shift amount cannot be negative, got $rhs")
      EvaluatedJValue.JNum(src, (ctx.expectNum(left).double.toLong << shamt).toDouble)

  case JValue.JUnaryOp(src, op, rawOperand) =>
    op match
    case JUnaryOperator.Op_! =>
      val operand = ctx.expectBoolean(rawOperand)
      EvaluatedJValue.JBoolean(src, !operand.value)
    case JUnaryOperator.Op_+  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(src, operand)
    case JUnaryOperator.Op_-  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(src, -operand)
    case JUnaryOperator.Op_~  =>
      val operand = ctx.expectNum(rawOperand).double
      EvaluatedJValue.JNum(src, (~operand.toLong).toDouble)
  case JValue.JLocal(_, name, value, result) =>
    evalUnsafe(ctx.bind(name, value))(result)
  case JValue.JFunction(src, params, body) =>
    EvaluatedJValue.JFunction(src, params.size, applyArgs(ctx, src, params, body))
  case JValue.JIf(src, rawCond, trueValue, elseValue) =>
    val cond = ctx.expectBoolean(rawCond)
    if cond.value then
      evalUnsafe(ctx)(trueValue)
    else
      elseValue.fold(EvaluatedJValue.JNull(src))(evalUnsafe(ctx))
  case JValue.JError(src, rawExpr) =>
    val msg = ctx.expectString(rawExpr)
    ctx.error(src, msg.str)
  case JValue.JAssert(src, rawCond, rawMsg, expr) =>
    val cond = ctx.expectBoolean(rawCond)
    if !cond.value then
      val msg = rawMsg.map(msg => ctx.expectString(msg).str)
      ctx.error(src, msg.getOrElse(s"assertion failed"))
    evalUnsafe(ctx)(expr)
  case JValue.JImport(src, file) =>
    ctx.importFile(src, file)
  case JValue.JImportStr(src, file) =>
    ctx.importStr(src, file)
  case JValue.JArrayComprehension(src, forVar, forExpr, inExpr, condOpt) =>
    if condOpt.isDefined then
      val cond = condOpt.get
      EvaluatedJValue.JArray(src, ctx.expectArray(inExpr).elements.flatMap { e =>
        val forCtx = ctx.bindEvaluated(forVar, e)
        Option.when(forCtx.expectBoolean(cond).value) {
          evalUnsafe(forCtx)(forExpr)
        }
      })
    else
      EvaluatedJValue.JArray(src, ctx.expectArray(inExpr).elements.map { e =>
        evalUnsafe(ctx.bindEvaluated(forVar, e))(forExpr)
      })

object Std:

  private val ctx = new EvaluationContext.Imp(
    SourceFile.std,
    Map.empty,
    List.empty,
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
        if positionalArgs.nonEmpty then
          if namedArgs.contains(argName) then ctx.error(positionalArgs.head.src, s"multiple values provided for argument $argName")
          evalUnsafe(ctx)(positionalArgs.head)
        else if namedArgs.contains(argName) then
          evalUnsafe(ctx)(namedArgs(argName))
        else if defaultOpt.isDefined then
          defaultOpt.get
        else
         ctx.error(params.src, s"no argument provided for $argName")
      i += 1
    result

  object temp:
    type ArgTuple[ArgNames] <: Tuple = ArgNames match
      case EmptyTuple => EmptyTuple
      case (arg *: tail) => EvaluatedJValue *: ArgTuple[tail]

    opaque type Arg[Name] = Option[EvaluatedJValue]

    object Arg:
      def apply[Name](default: EvaluatedJValue): Arg[Name] = Some(default)
      def apply[Name]: Arg[Name] = None

    extension [T](arg: Arg[T])
      def default: Option[EvaluatedJValue] = arg

  type Arg[T] = temp.Arg[T]
  val Arg = temp.Arg

  private inline def function1[Name1 <: String](
    arg1: temp.Arg[Name1] = temp.Arg[Name1],
    fn: (EvaluationContext, Source, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(
      Source.Generated,
      1,
      (applyCtx, params) => {
        val Array(arg) = bindArgs(Seq(
          compiletime.constValue[Name1] -> arg1.default
        ), applyCtx, params)
        fn(ctx, params.src, arg)
      }
    )

  private inline def function2[Name1 <: String, Name2 <: String](
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(
      Source.Generated,
      2,
      (applyCtx, params) => {
        val Array(arg1, arg2) = bindArgs(Seq(
          compiletime.constValue[Name1] -> None,
          compiletime.constValue[Name2] -> None,
        ), applyCtx, params)
        fn(ctx, params.src, arg1, arg2)
      }
    )

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

  val obj = makeObject(Map(
    "toString" -> function1(Arg["x"], toStringImp),
    "type" -> function1(Arg["x"], { (ctx, src, x) =>
      val value = EvaluationContext.typeString(x)
      EvaluatedJValue.JString(src, value)
    }),
    "length" -> function1(Arg["x"], { (ctx, src, x) =>
      val value = ctx.expectType[
        EvaluatedJValue.JArray
        | EvaluatedJValue.JString
        | EvaluatedJValue.JObject
        | EvaluatedJValue.JFunction
      ](x) match
      case e: EvaluatedJValue.JArray => e.elements.size
      case e: EvaluatedJValue.JString => e.str.size
      case e: EvaluatedJValue.JObject => e.members().size
      case e: EvaluatedJValue.JFunction => e.numParams
      EvaluatedJValue.JNum(src, value)
    })
  ))
