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

enum JBinaryOperator(val precedence: Int, val isLeftAssociative: Boolean):
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
        require(op(1) == 'n')
        Op_in
      case '=' =>
        require(op(1) == '=')
        Op_==
      case '!' =>
        require(op(1) == '=')
        Op_!=
      case '&' =>
        if op.size == 1 then Op_&
        else
          require(op(1) == '&')
          Op_&&
      case '|' =>
        if op.size == 1 then Op_|
        else
          require(op(1) == '|')
          Op_||

enum JObjMember:
  case JLocal(name: String, value: JValue)
  case JField(key: JValue, isHidden: Boolean, value: JValue)
  case JAssert(cond: JValue, expr: Option[JValue])

enum JValue:
  case JFalse
  case JTrue
  case JNull
  case JSelf
  case JSuper
  case JOuter
  case JString(str: String)
  case JNum(str: String)
  case JArray(elements: Seq[JValue])
  case JObject(members: Seq[JObjMember])
  case JObjectComprehension(
    preLocals: Seq[JObjMember.JLocal],
    comp: JObjMember.JField,
    postLocals: Seq[JObjMember.JLocal],
    inExprs: Seq[JValue],
    cond: Option[JValue],
  )
  case JId(name: String)
  case JGetField(loc: JValue, field: JId)
  case JIndex(loc: JValue, index: JValue)
  case JApply(loc: JValue, args: Seq[(Option[JId], JValue)])
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
  case JArrayComprehension(forVar: String, forExpr: JValue, inExpr: JValue, cond: Option[JValue])

type JParamList = Seq[(JValue.JId, Option[JValue])]
