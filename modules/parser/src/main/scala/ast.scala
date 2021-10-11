package root

import scala.annotation.switch

enum JUnaryOperator:
  case Op_!
  case Op_+
  case Op_-
  case Op_~

object JUnaryOperator:
  import scala.quoted.*

  given ToExpr[JUnaryOperator] with
    def apply(op: JUnaryOperator)(using Quotes): Expr[JUnaryOperator] =
      op match
      case Op_! => '{ Op_! }
      case Op_+ => '{ Op_+ }
      case Op_- => '{ Op_- }
      case Op_~ => '{ Op_~ }

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
  import scala.quoted.*

  given ToExpr[JBinaryOperator] with
    def apply(op: JBinaryOperator)(using Quotes): Expr[JBinaryOperator] =
      op match
      case Op_*  => '{ Op_*  }
      case Op_/  => '{ Op_/  }
      case Op_%  => '{ Op_%  }
      case Op_+  => '{ Op_+  }
      case Op_-  => '{ Op_-  }
      case Op_<< => '{ Op_<< }
      case Op_>> => '{ Op_>> }
      case Op_<  => '{ Op_<  }
      case Op_>  => '{ Op_>  }
      case Op_<= => '{ Op_<= }
      case Op_>= => '{ Op_>= }
      case Op_in => '{ Op_in }
      case Op_== => '{ Op_== }
      case Op_!= => '{ Op_!= }
      case Op_&  => '{ Op_&  }
      case Op_^  => '{ Op_^  }
      case Op_|  => '{ Op_|  }
      case Op_&& => '{ Op_&& }
      case Op_|| => '{ Op_|| }

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
  val std: SourceFile = new SourceFile:
    val path = "<std>"
    def getLineCol(offset: Int) = (0, offset)

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

  def merge(other: Source): Source =
    (this, other) match
    case (Range(start, _), Range(_, end)) => Range(start, end)
    case _ => this

object Source:
  import scala.quoted.*

  given ToExpr[Source] with
    def apply(src: Source)(using Quotes): Expr[Source] =
      src match
      case Range(start, end) => '{ Range(${Expr(start)}, ${Expr(end)}) }
      case Generated => '{ Generated }

  def apply(start: Int, end: Int): Source.Range =
    Source.Range(start, end)

enum JObjMember:
  case JLocal(src: Source, name: String, value: JValue)
  case JField(src: Source, key: JValue, plus: Boolean, isHidden: Boolean, value: JValue)
  case JAssert(src: Source, cond: JValue, msg: Option[JValue])

object JObjMember:
  import scala.quoted.*

  given ToExpr[JLocal] with
    def apply(local: JLocal)(using Quotes): Expr[JLocal] =
       '{ JLocal(${Expr(local.src)}, ${Expr(local.name)}, ${Expr(local.value)}) }

  given ToExpr[JObjMember] with
    def apply(member: JObjMember)(using Quotes): Expr[JObjMember] =
      member match
      case JLocal(src, name, value) => '{ JLocal(${Expr(src)}, ${Expr(name)}, ${Expr(value)}) }
      case JField(src, key, plus, isHidden, value) => '{ JField(${Expr(src)}, ${Expr(key)}, ${Expr(plus)}, ${Expr(isHidden)}, ${Expr(value)}) }
      case JAssert(src, cond, msg) => '{ JAssert(${Expr(src)}, ${Expr(cond)}, ${Expr(msg)}) }

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

object JValue:
  import scala.quoted.*

  given ToExpr[JValue] with
    def apply(jvalue: JValue)(using Quotes): Expr[JValue] =
      jvalue match
      case JFalse(src) => '{ JFalse(${Expr(src)}) }
      case JTrue(src) => '{ JTrue(${Expr(src)}) }
      case JNull(src) => '{ JNull(${Expr(src)}) }
      case JSelf(src) => '{ JSelf(${Expr(src)}) }
      case JSuper(src) => '{ JSuper(${Expr(src)}) }
      case JOuter(src) => '{ JOuter(${Expr(src)}) }
      case JString(src, str) => '{ JString(${Expr(src)}, ${Expr(str)}) }
      case JNum(src, str) => '{ JNum(${Expr(src)}, ${Expr(str)}) }
      case JArray(src, elements) => '{ JArray(${Expr(src)}, ${Expr.ofSeq(elements.map(Expr(_)))}) }
      case JObject(src, members) => '{ JObject(${Expr(src)}, ${Expr.ofSeq(members.map(Expr(_)))}) }
      case JObjectComprehension(src, preLocals, key, value, postLocals, forVar, inExpr, cond) => '{
        JObjectComprehension(
          ${Expr(src)},
          ${Expr.ofSeq(preLocals.map(Expr(_)))},
          ${Expr(key)},
          ${Expr(value)},
          ${Expr.ofSeq(postLocals.map(Expr(_)))},
          ${Expr(forVar)},
          ${Expr(inExpr)},
          ${Expr(cond)},
        )
      }
      case JId(src, name) => '{ JId(${Expr(src)}, ${Expr(name)}) }
      case JGetField(src, loc, field) => '{ JGetField(${Expr(src)}, ${Expr(loc)}, ${Expr(field)}) }
      case JIndex(src, loc, index) => '{ JIndex(${Expr(src)}, ${Expr(loc)}, ${Expr(index)}) }
      case JSlice(src, loc, index, endIndex, stride) => '{ JSlice(${Expr(src)}, ${Expr(loc)}, ${Expr(index)}, ${Expr(endIndex)}, ${Expr(stride)}) }
      case JApply(src, loc, positionalArgs, namedArgs) => '{
        JApply(
          ${Expr(src)},
          ${Expr(loc)},
          ${Expr.ofSeq(positionalArgs.map(Expr(_)))},
          ${Expr.ofSeq(namedArgs.map { (name, arg) =>
            '{ ${Expr(name)} -> ${Expr(arg)} }
          })}
        )
      }
      case JBinaryOp(src, left, op, right) => '{ JBinaryOp(${Expr(src)}, ${Expr(left)}, ${Expr(op)}, ${Expr(right)}) }
      case JUnaryOp(src, op, expr) => '{ JUnaryOp(${Expr(src)}, ${Expr(op)}, ${Expr(expr)}) }
      case JLocal(src, name, value, result) => '{ JLocal(${Expr(src)}, ${Expr(name)}, ${Expr(value)}, ${Expr(result)}) }
      case JFunction(src, params, body) => '{ JFunction(${Expr(src)}, ${Expr(params)}, ${Expr(body)}) }
      case JIf(src, cond, trueValue, elseValue) => '{ JIf(${Expr(src)}, ${Expr(cond)}, ${Expr(trueValue)}, ${Expr(elseValue)}) }
      case JError(src, expr) => '{ JError(${Expr(src)}, ${Expr(expr)}) }
      case JAssert(src, cond, msg, expr) => '{ JAssert(${Expr(src)}, ${Expr(cond)}, ${Expr(msg)}, ${Expr(expr)}) }
      case JImport(src, file) => '{ JImport(${Expr(src)}, ${Expr(file)}) }
      case JImportStr(src, file) => '{ JImportStr(${Expr(src)}, ${Expr(file)}) }
      case JArrayComprehension(src, forVar, forExpr, inExpr, cond) => '{
        JArrayComprehension(
          ${Expr(src)},
          ${Expr(forVar)},
          ${Expr(forExpr)},
          ${Expr(inExpr)},
          ${Expr(cond)},
        )
      }

  private def readFileString(relativeFilename: Expr[String])(using quotes: Quotes): String =
    import quotes.reflect.{report, Position}
    import quotes.reflect.given
    val currPath = Position
      .ofMacroExpansion
      .sourceFile
      .jpath

    if currPath == null then
      report.throwError("could not find file of macro expansion location", Position.ofMacroExpansion)

    val file =
      currPath
      .toAbsolutePath
      .getParent
      .resolve(relativeFilename.valueOrError)

    if !java.nio.file.Files.exists(file) then
      report.throwError(s"file $file does not exist", Position.ofMacroExpansion)

    java.nio.file.Files.readString(file)

  inline def readFile(inline relativeFilename: String): String =
    ${ readFileImpl('relativeFilename) }

  def readFileImpl(relativeFilename: Expr[String])(using quotes: Quotes): Expr[String] =
    import quotes.reflect.{report, Position}
    Expr(readFileString(relativeFilename))

  inline def reifyFile(inline relativeFilename: String): JValue =
    ${ reifyFileIml('relativeFilename) }

  def reifyFileIml(relativeFilename: Expr[String])(using quotes: Quotes): Expr[JValue] =
    import quotes.reflect.{report, Position}
    import quotes.reflect.given
    Parser.parserFile.parseAll(readFileString(relativeFilename)).fold(
      err => report.throwError(err.toString, Position.ofMacroExpansion),
      jvalue => Expr(jvalue),
    )

  inline def reify(inline string: String): JValue =
    ${ reifyIml('string) }

  def reifyIml(string: Expr[String])(using quotes: Quotes): Expr[JValue] =
    import quotes.reflect._
    val literal =
      string.asTerm match
      case Literal(StringConstant(x)) => x
      case _ => string.valueOrError
    Parser.parserFile.parseAll(literal).fold(
      err => report.throwError(err.toString, Position.ofMacroExpansion),
      jvalue => Expr(jvalue),
    )

type JParamList = Seq[(String, Option[JValue])]
