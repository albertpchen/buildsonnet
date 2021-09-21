package root

import scala.annotation.switch

enum JUnaryOperator:
  case Op_!
  case Op_+
  case Op_-
  case Op_~

object JUnaryOperator:
  def apply(op: Char): JUnaryOperator =
    (op: @switch) match
      case '!' => Op_!
      case '+' => Op_+
      case '-' => Op_-
      case '~' => Op_~

enum JBinaryOperator(
  val precedence: Int,
  val isLeftAssociative: Boolean,
):
  case Op_*  extends JBinaryOperator(12, true)
  case Op_/  extends JBinaryOperator(12, true)
  case Op_%  extends JBinaryOperator(12, true)
  case Op_+  extends JBinaryOperator(11, true)
  case Op_-  extends JBinaryOperator(11, true)
  case Op_<< extends JBinaryOperator(10, true)
  case Op_>> extends JBinaryOperator(10, true)
  case Op_<  extends JBinaryOperator(9, true)
  case Op_>  extends JBinaryOperator(9, true)
  case Op_<= extends JBinaryOperator(9, true)
  case Op_>= extends JBinaryOperator(9, true)
  case Op_in extends JBinaryOperator(9, true)
  case Op_== extends JBinaryOperator(8, true)
  case Op_!= extends JBinaryOperator(8, true)
  case Op_&  extends JBinaryOperator(7, true)
  case Op_^  extends JBinaryOperator(6, true)
  case Op_|  extends JBinaryOperator(5, true)
  case Op_&& extends JBinaryOperator(4, true)
  case Op_|| extends JBinaryOperator(4, true)

object JBinaryOperator:
  def apply(op: String): JBinaryOperator =
    (op(0): @switch) match
      case '*' => Op_*
      case '/' => Op_/
      case '%' => Op_%
      case '+' => Op_+
      case '-' => Op_-
      case '<' =>
        if op.size == 1 then Op_<
        else
          (op(1): @switch) match
            case '=' => Op_<=
            case '<' => Op_<<
      case '>' =>
        if op.size == 1 then Op_<
        else
          (op(1): @switch) match
            case '=' => Op_>=
            case '>' => Op_>>
      case 'i' =>
        require(op.size == 2 && op(1) == 'n', s"invalid operator '$op'")
        Op_in
      case '=' =>
        require(op.size == 2 && op(1) == '=', s"invalid operator '$op'")
        Op_==
      case '!' =>
        require(op.size == 2 && op(1) == '=', s"invalid operator '$op'")
        Op_!=
      case '&' =>
        if op.size == 1 then Op_&
        else
          require(op.size == 2 && op(1) == '&', s"invalid operator '$op'")
          Op_&&
      case '|' =>
        if op.size == 1 then Op_|
        else
          require(op.size == 2 && op(1) == '|', s"invalid operator '$op'")
          Op_||

enum Source:
  case Range(start: Int, end: Int)
  case Generated

object Source:
  def apply(start: Int, end: Int): Source.Range =
    Source.Range(start, end)

enum JObjMember:
  case JLocal(name: String, value: JValue)
  case JField(key: JValue, plus: Boolean, isHidden: Boolean, value: JValue)
  case JAssert(cond: JValue, msg: Option[JValue])

enum JValue:
  case JFalse(src: Source)
  case JTrue(src: Source)
  case JNull(src: Source)
  case JSelf(src: Source)
  case JSuper(src: Source)
  case JOuter(src: Source)
  case JString(src: Source, str: String)
  case JNum(src: Source, str: String)
  case JArray(src: Source, elements: Seq[JValue])
  case JObject(src: Source, members: Seq[JObjMember])
  case JObjectComprehension(
    src: Source,
    preLocals: Seq[JObjMember.JLocal],
    key: JValue,
    value: JValue,
    postLocals: Seq[JObjMember.JLocal],
    forVar: String,
    inExpr: JValue,
    cond: Option[JValue],
  )
  case JId(src: Source, name: String)
  case JGetField(src: Source, loc: JValue, field: String)
  case JIndex(src: Source, loc: JValue, index: JValue)
  case JSlice(src: Source, loc: JValue, index: JValue, endIndex: Option[JValue], stride: Option[JValue])
  case JApply(src: Source, loc: JValue, positionalArgs: Seq[JValue], namedArgs: Seq[(String, JValue)])
  case JBinaryOp(left: JValue, op: JBinaryOperator, right: JValue)
  case JUnaryOp(op: JUnaryOperator, expr: JValue)
  case JLocal(name: String, value: JValue, result: JValue)
  case JArrComprehension(comp: JValue, inExprs: Seq[JValue], cond: Option[JValue])
  case JFunction(params: JParamList, body: JValue)
  case JIf(src: Source, cond: JValue, trueValue: JValue, elseValue: Option[JValue])
  case JError(expr: JValue)
  case JAssert(cond: JValue, msg: Option[JValue], expr: JValue)
  case JImport(file: String)
  case JImportStr(file: String)
  case JArrayComprehension(
    src: Source,
    forVar: String,
    forExpr: JValue,
    inExpr: JValue,
    cond: Option[JValue]
  )

  def isNull: Boolean = this match
    case _: JNull => true
    case _ => false

type JParamList = Seq[(String, Option[JValue])]
