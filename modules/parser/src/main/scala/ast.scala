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

sealed trait SourceFile:
  def path: String
  def getLineCol(offset: Int): (Int, Int)

object SourceFile:
  val empty: SourceFile = new SourceFile:
    val path = "<empty>"
    def getLineCol(offset: Int) = (0, offset)

  private class BinTree(
    val index: Int,
    val value: Int,
    left: Option[BinTree],
    right: Option[BinTree]
  ):
    def find(i: Int): BinTree =
      if value < i then
        if right.isEmpty then
          this
        else
          val foundRight = right.get.find(i)
          if foundRight.value < i then
            if (i - value) < (i - foundRight.value) then
              this
            else
              foundRight
          else
            this
      else if value > i then
        if left.isEmpty then
          this
        else
          left.get.find(i)
      else
        this
  private def makeBinTree(
    ints: collection.IndexedSeq[Int],
    begin: Int,
    end: Int
  ): BinTree =
    val span = end - begin
    if span == 1 then
      new BinTree(begin, ints(begin), None, None)
    else if span == 2 then
      new BinTree(
        begin,
        ints(begin),
        None,
        Some(new BinTree(begin + 1, ints(begin + 1), None, None))
      )
    else
      val halfSpan = (span - 1) / 2
      val halfIndex = begin + halfSpan
      val left = makeBinTree(ints, begin, halfIndex)
      val right = makeBinTree(ints, halfIndex + 1, end)
      new BinTree(
        halfIndex,
        ints(halfIndex),
        Some(left),
        Some(right),
      )

  def apply(file: String, contents: String): SourceFile =
    val indices = collection.mutable.ArrayBuffer[Int]()
    indices += -1
    for
      i <- 0 until contents.size
    do
      if contents(i) == '\n' then
        indices += i
    if indices.size <= 1 then
      new SourceFile:
        val path = file
        def getLineCol(offset: Int) = (0, offset)
    else
      val binTree = makeBinTree(indices, 0, indices.size)
      new SourceFile:
        val path = file
        def getLineCol(offset: Int) =
          val closest = binTree.find(offset)
          val col = offset - closest.value
          (closest.index + 1, col)


enum Source:
  case Range(start: Int, end: Int)
  case Generated

  def withStart(newStart: Int): Source =
    this match
    case Range(start, end) => Range(newStart, end)
    case Generated => Generated

  def withEnd(newEnd: Int): Source =
    this match
    case Range(start, end) => Range(start, newEnd)
    case Generated => Generated

object Source:
  def apply(start: Int, end: Int): Source.Range =
    Source.Range(start, end)

enum JObjMember:
  case JLocal(src: Source, name: String, value: JValue)
  case JField(src: Source, key: JValue, plus: Boolean, isHidden: Boolean, value: JValue)
  case JAssert(src: Source, cond: JValue, msg: Option[JValue])

trait HasSource:
  def src: Source

enum JValue extends HasSource:
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
  case JBinaryOp(src: Source, left: JValue, op: JBinaryOperator, right: JValue)
  case JUnaryOp(src: Source, op: JUnaryOperator, expr: JValue)
  case JLocal(src: Source, name: String, value: JValue, result: JValue)
  case JFunction(src: Source, params: JParamList, body: JValue)
  case JIf(src: Source, cond: JValue, trueValue: JValue, elseValue: Option[JValue])
  case JError(src: Source, expr: JValue)
  case JAssert(src: Source, cond: JValue, msg: Option[JValue], expr: JValue)
  case JImport(src: Source, file: String)
  case JImportStr(src: Source, file: String)
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
