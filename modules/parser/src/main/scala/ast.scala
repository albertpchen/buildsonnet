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

  def withStart(newStart: Int): Source =
    this match
    case Range(start, end) => Range(newStart, end)
    case Generated => Generated

object Source:
  def apply(start: Int, end: Int): Source.Range =
    Source.Range(start, end)

enum JObjMember:
  case JLocal(src: Source, name: String, value: JValue)
  case JField(src: Source, key: JValue, plus: Boolean, isHidden: Boolean, value: JValue)
  case JAssert(src: Source, cond: JValue, msg: Option[JValue])

sealed trait HasSource:
  def src: Source

enum JValue(src: Source) extends HasSource:
  case JFalse(src: Source) extends JValue(src)
  case JTrue(src: Source) extends JValue(src)
  case JNull(src: Source) extends JValue(src)
  case JSelf(src: Source) extends JValue(src)
  case JSuper(src: Source) extends JValue(src)
  case JOuter(src: Source) extends JValue(src)
  case JString(src: Source, str: String) extends JValue(src)
  case JNum(src: Source, str: String) extends JValue(src)
  case JArray(src: Source, elements: Seq[JValue]) extends JValue(src)
  case JObject(src: Source, members: Seq[JObjMember]) extends JValue(src)
  case JObjectComprehension(
    src: Source,
    preLocals: Seq[JObjMember.JLocal],
    key: JValue,
    value: JValue,
    postLocals: Seq[JObjMember.JLocal],
    forVar: String,
    inExpr: JValue,
    cond: Option[JValue],
  ) extends JValue(src)
  case JId(src: Source, name: String) extends JValue(src)
  case JGetField(src: Source, loc: JValue, field: String) extends JValue(src)
  case JIndex(src: Source, loc: JValue, index: JValue) extends JValue(src)
  case JSlice(src: Source, loc: JValue, index: JValue, endIndex: Option[JValue], stride: Option[JValue]) extends JValue(src)
  case JApply(src: Source, loc: JValue, positionalArgs: Seq[JValue], namedArgs: Seq[(String, JValue)]) extends JValue(src)
  case JBinaryOp(src: Source, left: JValue, op: JBinaryOperator, right: JValue) extends JValue(src)
  case JUnaryOp(src: Source, op: JUnaryOperator, expr: JValue) extends JValue(src)
  case JLocal(src: Source, name: String, value: JValue, result: JValue) extends JValue(src)
  case JFunction(src: Source, params: JParamList, body: JValue) extends JValue(src)
  case JIf(src: Source, cond: JValue, trueValue: JValue, elseValue: Option[JValue]) extends JValue(src)
  case JError(src: Source, expr: JValue) extends JValue(src)
  case JAssert(src: Source, cond: JValue, msg: Option[JValue], expr: JValue) extends JValue(src)
  case JImport(src: Source, file: String) extends JValue(src)
  case JImportStr(src: Source, file: String) extends JValue(src)
  case JArrayComprehension(
    src: Source,
    forVar: String,
    forExpr: JValue,
    inExpr: JValue,
    cond: Option[JValue]
  ) extends JValue(src)

  def isNull: Boolean = this match
    case _: JNull => true
    case _ => false

type JParamList = Seq[(String, Option[JValue])]
