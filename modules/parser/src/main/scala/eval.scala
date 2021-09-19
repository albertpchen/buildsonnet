package root

sealed trait EvaluationContext:
  def error(message: String): Nothing
  def lookup(id: String): EvaluatedJValue
  def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext
  def functionCtx(fn: EvaluatedJValue.JFunction): EvaluationContext
  def bind(id: String, value: JValue): EvaluationContext
  def importFile(fileName: String): EvaluatedJValue
  def importStr(fileName: String): EvaluatedJValue.JString
  def self: EvaluatedJValue.JObject
  def `super`: EvaluatedJValue.JObject
  def hasSuper: Boolean

sealed trait ObjectEvaluationContext extends EvaluationContext:
  def bind(id: String, value: JValue): ObjectEvaluationContext
  def withSelf(self: EvaluatedJValue.JObject): ObjectEvaluationContext
  def withParent(self: EvaluatedJValue.JObject): ObjectEvaluationContext

sealed trait LazyValue:
  lazy val evaluated: EvaluatedJValue

sealed trait LazyObjectValue extends LazyValue:
  val isHidden: Boolean
  // def withSelf(self: EvaluatedJValue.JObject): LazyObjectValue
  // def withParent(parent: EvaluatedJValue.JObject): LazyObjectValue

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
        //println("SDFLKJ: " + code.toString)
        eval(ctx)(code)
      }

      // def withSelf(self: EvaluatedJValue.JObject): LazyObjectValue =
      //   LazyValue(ctx.withSelf(self), code, hidden)

      // def withParent(parent: EvaluatedJValue.JObject): LazyObjectValue =
      //   LazyValue(ctx.withParent(parent), code, hidden)

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
    inline def typeError2[T1 <: EvaluatedJValue, T2 <: EvaluatedJValue](expr: EvaluatedJValue): T1 | T2 =
      ctx.error(s"got type ${typeString(expr)}, expected ${typeString[T1]} or ${typeString[T2]}")

    inline def typeError[T <: EvaluatedJValue](expr: EvaluatedJValue): T =
      ctx.error(s"got type ${typeString(expr)}, expected ${typeString[T]}")

    inline def expectType[T <: EvaluatedJValue](expr: EvaluatedJValue): T =
      if expr.isInstanceOf[T] then
        expr.asInstanceOf[T]
      else
        typeError[T](expr)

  private class Imp(
    val scope: Map[String, LazyValue],
    val stack: List[EvaluatedJValue.JFunction | EvaluatedJValue.JObject],
  ) extends EvaluationContext:
    def error(message: String): Nothing = throw new Exception(message)

    def lookup(id: String): EvaluatedJValue =
      scope.get(id).fold {
        error(s"no variable $id defined")
      }(_.evaluated)

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        obj,
        None,
        this,
        Map.empty,
        stack,
      ).bind("$", JValue.JSelf)

    def functionCtx(fn: EvaluatedJValue.JFunction): Imp =
      new Imp(scope, fn +: stack)

    def bind(id: String, value: JValue): Imp =
      val newScope = scope + (id -> LazyValue(this, value))
      new Imp(newScope, stack)

    def importFile(fileName: String): EvaluatedJValue = ???
    def importStr(fileName: String): EvaluatedJValue.JString = ???

    def self: EvaluatedJValue.JObject = error("no self")
    def `super`: EvaluatedJValue.JObject = error("no super")
    def hasSuper: Boolean = false

  private case class ObjectImp(
    override val self: EvaluatedJValue.JObject,
    superOpt: Option[EvaluatedJValue.JObject],
    topCtx: Imp,
    locals: Map[String, JValue],
    stack: List[EvaluatedJValue.JFunction | EvaluatedJValue.JObject],
  ) extends ObjectEvaluationContext:

    def `super` = superOpt.getOrElse(topCtx.`super`)
    def hasSuper: Boolean = superOpt.isDefined
    export topCtx.error, topCtx.importStr, topCtx.importFile

    @scala.annotation.threadUnsafe
    lazy val cache = collection.mutable.HashMap[String, EvaluatedJValue]()
    val scope = locals.map { (id, value) =>
      id -> LazyValue(this, value)
    }
    def lookup(id: String): EvaluatedJValue =
      if scope.contains(id) then
        scope(id).evaluated
      else
        topCtx.lookup(id)

    def objectCtx(obj: EvaluatedJValue.JObject): ObjectEvaluationContext =
      new ObjectImp(
        obj,
        None,
        new Imp(topCtx.scope ++ scope, topCtx.stack),
        Map.empty,
        stack,
      )

    def functionCtx(fn: EvaluatedJValue.JFunction) =
      this.copy(stack = fn +: stack)

    def withSelf(newSelf: EvaluatedJValue.JObject) =
      if `self` == newSelf then this else this.copy(`self` = newSelf)

    def withParent(parent: EvaluatedJValue.JObject) =
      if superOpt.isDefined && superOpt.get == parent then
        this
      else
        this.copy(superOpt = Some(parent))

    def bind(id: String, value: JValue) =
      this.copy(locals = locals + (id -> value))

  def apply(): EvaluationContext = Imp(Map.empty, List.empty)

case class ManifestError(msg: String)

enum ManifestedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[ManifestedJValue])
  case JObject(members: Map[String, ManifestedJValue])

  override def toString: String =
    val builder = new StringBuilder
    prettyPrintImp("  ", 0, None, builder)
    builder.toString

  private def prettyPrintImp(
    tab: String,
    tabNum: Int,
    firstPrefix: Option[String],
    builder: StringBuilder
  ): Unit =
    val prefix = tab * tabNum
    builder ++= firstPrefix.getOrElse(prefix)
    this match
      case JNull => builder ++= "null"
      case JString(value) =>
        builder += '"'
        ManifestedJValue.escape(value, builder)
        builder += '"'
      case JNum(value) =>
        if value.isWhole then
          builder ++= value.toLong.toString
        else
          builder ++= value.toString
      case JBoolean(value) => builder ++= value.toString
      case JArray(value) if value.isEmpty => builder ++= "[]"
      case JArray(value) =>
        builder ++= "[\n"
        value.head.prettyPrintImp(tab, tabNum + 1, None, builder)
        value.tail.foreach { e =>
          builder ++= ",\n"
          e.prettyPrintImp(tab, tabNum + 1, None, builder)
        }
        builder += '\n'
        builder ++= prefix
        builder += ']'
      case JObject(value) if value.isEmpty => builder ++= "{}"
      case JObject(unsorted) =>
        val value = unsorted.toSeq.sortBy(_._1)
        builder ++= "{\n"
        builder ++= prefix
        builder ++= tab
        builder += '"'
        ManifestedJValue.escape(value.head._1, builder)
        value.head._2.prettyPrintImp(tab, tabNum + 1, Some("\": "), builder)
        value.tail.foreach { case (k, v) =>
          builder ++= ",\n"
          builder ++= prefix
          builder ++= tab
          builder += '"'
          ManifestedJValue.escape(k, builder)
          v.prettyPrintImp(tab, tabNum + 1, Some("\": "), builder)
        }
        builder += '\n'
        builder ++= prefix
        builder += '}'

object ManifestedJValue:
  private def escape(s: String, builder: StringBuilder): Unit =
    var idx = 0
    val len = s.length
    while idx < len do
      (s(idx): @annotation.switch) match
        case '"'  => builder ++= "\\\""
        case '\\' => builder ++= "\\\\"
        case '\b' => builder ++= "\\b"
        case '\f' => builder ++= "\\f"
        case '\n' => builder ++= "\\n"
        case '\r' => builder ++= "\\r"
        case '\t' => builder ++= "\\t"
        case c =>
          val shouldEscape = (c >= '\u0000' && c <= '\u001f')
          || (c >= '\u0080' && c < '\u00a0')
          || (c >= '\u2000' && c < '\u2100')
          if shouldEscape then
            builder ++= "\\u%04x".format(c: Int)
          else
            builder ++= c.toString
      idx += 1

sealed trait JObjectImp(
  ctxThunk: () => ObjectEvaluationContext,
  rawMembers: Seq[JObjMember.JField],
):
  private def ctx = ctxThunk()

  private var _cache: collection.mutable.HashMap[String, LazyObjectValue] = null
  lazy val cache = {
    val result = collection.mutable.HashMap[String, LazyObjectValue]()
    rawMembers.foreach { case JObjMember.JField(key, isHidden, value) =>
      result(ctx.expectType[EvaluatedJValue.JString](eval(ctx)(key)).str) = LazyValue(ctx, value, isHidden)
    }
    result
  }

  def lookup(field: String): EvaluatedJValue =
    cache.getOrElse(field, ctx.error(s"object missing field $field")).evaluated

  def members(): collection.Map[String, LazyObjectValue] =
    if ctx.hasSuper then
      ctx.`super`.members() ++ cache
    else
      cache

enum EvaluatedJValue:
  case JBoolean(value: Boolean)
  case JNull
  case JString(str: String)
  case JNum(double: Double)
  case JArray(elements: Seq[EvaluatedJValue])
  /** needs to handle:
    *
    * - simple lookup, defined in current object
    * - super lookup (dynamic), not in current object, but in super object
    * - self lookup (dynamic), lookup in current object, possibly in super
    */
  case JObject(ctx: () => ObjectEvaluationContext, rawMembers: Seq[JObjMember.JField]) extends EvaluatedJValue with JObjectImp(ctx, rawMembers)
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
    case obj: JObject =>
      obj.members().toSeq.sortBy(_._1).foldLeft(Right(Seq.empty):Either[String, Seq[(String, ManifestedJValue)]] ) {
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
    //println(jvalue)
    jvalue match
    case JValue.JFalse => EvaluatedJValue.JBoolean(false)
    case JValue.JTrue => EvaluatedJValue.JBoolean(true)
    case JValue.JNull => EvaluatedJValue.JNull
    case JValue.JSelf => ctx.self
    case JValue.JSuper => ctx.`super`
    case JValue.JOuter => ctx.lookup("$")
    case JValue.JString(str) => EvaluatedJValue.JString(str)
    case JValue.JNum(str) => EvaluatedJValue.JNum(str.toDouble)
    case JValue.JArray(elements) => EvaluatedJValue.JArray(elements.map(apply(ctx)))
    case JValue.JObject(members) =>
      var objMembers: Map[String, LazyObjectValue] = null
      var objCtx: ObjectEvaluationContext = null
      val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(() => objCtx, members.collect {
        case f: JObjMember.JField => f
      })
      objCtx = members.foldLeft(ctx.objectCtx(obj)) {
        case (ctx, local: JObjMember.JLocal) => ctx.bind(local.name, local.value)
        case (ctx, _) => ctx
      }

      // bind all locals inside object - using the current context, not the object context
      objMembers = members.collect {
        case JObjMember.JField(rawKey, isHidden, value) =>
          val key = ctx.expectType[EvaluatedJValue.JString](eval(ctx)(rawKey)).str
          key -> LazyValue(objCtx, value, isHidden)
      }.toMap

      // evaluate asserts inside object
      members.foreach {
        case JObjMember.JAssert(rawCond, rawMsg) =>
          val cond = objCtx.expectType[EvaluatedJValue.JBoolean](eval(objCtx)(rawCond)).value
          val msgOpt = rawMsg.map(msg => objCtx.expectType[EvaluatedJValue.JString](eval(objCtx)(msg)).str)
          if !cond then objCtx.error(msgOpt.getOrElse("object assertion failed"))
        case _ =>
      }

      obj
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
        (apply(ctx)(left), apply(ctx)(right)) match
        case (op1: EvaluatedJValue.JNum, op2: EvaluatedJValue.JNum) =>
          EvaluatedJValue.JNum(op1.double + op2.double)
        case (op1: EvaluatedJValue.JObject, op2: EvaluatedJValue.JObject) =>
          var members: Map[String, LazyObjectValue] = null
          var parentCtx: ObjectEvaluationContext = null
          val parent: EvaluatedJValue.JObject = {
            op1.copy(ctx = () => parentCtx)
          }
          var resultCtx: ObjectEvaluationContext = null
          val result: EvaluatedJValue.JObject = {
            op2.copy(ctx = () => resultCtx)
          }
          resultCtx = op2.ctx().withSelf(result).withParent(parent)
          parentCtx = op1.ctx().withSelf(result)
          result
        case (op1: EvaluatedJValue.JNum, op2) => ctx.typeError[EvaluatedJValue.JNum](op2)
        case (op1, op2: EvaluatedJValue.JNum) => ctx.typeError[EvaluatedJValue.JNum](op2)
        case (op1: EvaluatedJValue.JObject, op2) => ctx.typeError[EvaluatedJValue.JObject](op2)
        case (op1, op2: EvaluatedJValue.JObject) => ctx.typeError[EvaluatedJValue.JObject](op2)
        case (op1, _) =>
          ctx.typeError2[EvaluatedJValue.JNum, EvaluatedJValue.JObject](op1)

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
