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
        if op.size == 1 then Op_>
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
  def contents: String
  def getLineCol(offset: Int): (Int, Int)

object SourceFile:
  import scala.quoted.*

  given ToExpr[SourceFile] with
    def apply(src: SourceFile)(using Quotes): Expr[SourceFile] =
      '{ SourceFile(${Expr(src.path)}, ${Expr(src.contents)}) }

  val std: SourceFile = new SourceFile:
    val path = "<std>"
    val contents = ""
    def getLineCol(offset: Int) = (0, offset)

  val empty: SourceFile = new SourceFile:
    val path = "<empty>"
    val contents = ""
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

  def apply(file: String, contentsx: String): SourceFile =
    val indices = collection.mutable.ArrayBuffer[Int]()
    indices += -1
    for
      i <- 0 until contentsx.size
    do
      if contentsx(i) == '\n' then
        indices += i
    if indices.size <= 1 then
      new SourceFile:
        val path = file
        val contents = contentsx
        def getLineCol(offset: Int) = (0, offset)
    else
      val binTree = makeBinTree(indices, 0, indices.size)
      new SourceFile:
        val path = file
        val contents = contentsx
        def getLineCol(offset: Int) =
          val closest = binTree.find(offset)
          val col = offset - closest.value
          (closest.index + 1, col)

trait HasSourceFile:
  def file: SourceFile

enum Source extends HasSourceFile:
  case Range(file: SourceFile, start: Int, end: Int)
  case Generated(file: SourceFile)

  def setFile(file: SourceFile): Source =
    this match
    case r: Range => r.copy(file = file)
    case g: Generated => g.copy(file = file)

  def withStart(newStart: Int): Source =
    this match
    case r: Range => r.copy(start = newStart)
    case g: Generated => g

  def withEnd(newEnd: Int): Source =
    this match
    case r: Range => r.copy(end = newEnd)
    case g: Generated => g

  def merge(other: Source): Source =
    (this, other) match
    case (r1: Range, r2: Range) => Range(r1.file, r1.start, r2.end)
    case _ => this

object Source:
  import scala.quoted.*

  given ToExpr[Source] with
    def apply(src: Source)(using Quotes): Expr[Source] =
      src match
      case Range(file, start, end) => '{ Range(${Expr(file)}, ${Expr(start)}, ${Expr(end)}) }
      case Generated(file) => '{ Generated(${Expr(file)}) }

  val empty: Source = Generated(SourceFile.empty)

  def apply(start: Int, end: Int): Source.Range =
    Source.Range(SourceFile.empty, start, end)

enum JObjMember:
  case JLocal(src: Source, name: String, value: JValue)
  case JField(src: Source, key: JValue, plus: Boolean, isHidden: Boolean, value: JValue)
  case JAssert(src: Source, cond: JValue, msg: Option[JValue])

  def setFile(file: SourceFile): JObjMember =
    this match
    case v: JLocal => v.copy(src = v.src.setFile(file), value = v.value.setFile(file))
    case v: JField => v.copy(src = v.src.setFile(file), key = v.key.setFile(file), value = v.value.setFile(file))
    case v: JAssert => v.copy(src = v.src.setFile(file), cond = v.cond.setFile(file), msg = v.msg.map(_.setFile(file)))

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

  transparent inline def setFile[T <: JObjMember](member: T, file: SourceFile) =
    inline member match
    case v: JLocal => v.copy(src = v.src.setFile(file), value = v.value.setFile(file))
    case v: JField => v.copy(src = v.src.setFile(file), key = v.key.setFile(file), value = v.value.setFile(file))
    case v: JAssert => v.copy(src = v.src.setFile(file), cond = v.cond.setFile(file), msg = v.msg.map(_.setFile(file)))

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

  def setFile(file: SourceFile): JValue =
    this match
    case v: JFalse => v.copy(src = v.src.setFile(file))
    case v: JTrue => v.copy(src = v.src.setFile(file))
    case v: JNull => v.copy(src = v.src.setFile(file))
    case v: JSelf => v.copy(src = v.src.setFile(file))
    case v: JSuper => v.copy(src = v.src.setFile(file))
    case v: JOuter => v.copy(src = v.src.setFile(file))
    case v: JString => v.copy(src = v.src.setFile(file))
    case v: JNum => v.copy(src = v.src.setFile(file))
    case v: JArray => v.copy(src = v.src.setFile(file), elements = v.elements.map(_.setFile(file)))
    case v: JObject => v.copy(src = v.src.setFile(file), members = v.members.map(_.setFile(file)))
    case v: JObjectComprehension => v.copy(
      src = v.src.setFile(file),
      preLocals = v.preLocals.map(JObjMember.setFile(_, file)),
      key = v.key.setFile(file),
      value = v.value.setFile(file),
      postLocals = v.preLocals.map(JObjMember.setFile(_, file)),
      inExpr = v.inExpr.setFile(file),
      cond = v.cond.map(_.setFile(file)),
    )
    case v: JId => v.copy(src = v.src.setFile(file))
    case v: JGetField => v.copy(src = v.src.setFile(file), loc = v.loc.setFile(file))
    case v: JIndex => v.copy(src = v.src.setFile(file), loc = v.loc.setFile(file), index = v.index.setFile(file))
    case v: JSlice => v.copy(
      src = v.src.setFile(file),
      loc = v.loc.setFile(file),
      index = v.index.setFile(file),
      endIndex = v.endIndex.map(_.setFile(file)),
      stride = v.stride.map(_.setFile(file)),
    )
    case v: JApply => v.copy(src = v.src.setFile(file))
    case v: JBinaryOp => v.copy(src = v.src.setFile(file))
    case v: JUnaryOp => v.copy(src = v.src.setFile(file))
    case v: JLocal => v.copy(src = v.src.setFile(file))
    case v: JFunction => v.copy(src = v.src.setFile(file))
    case v: JIf => v.copy(src = v.src.setFile(file))
    case v: JError => v.copy(src = v.src.setFile(file))
    case v: JAssert => v.copy(src = v.src.setFile(file))
    case v: JImport => v.copy(src = v.src.setFile(file))
    case v: JImportStr => v.copy(src = v.src.setFile(file))
    case v: JArrayComprehension => v.copy(src = v.src.setFile(file))

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

  inline def reifyFile(inline relativeFilename: String, inline srcFilename: String): JValue =
    ${ reifyFileIml('relativeFilename, 'srcFilename) }

  def reifyFileIml(
    relativeFilename: Expr[String],
    srcFilename: Expr[String],
  )(using quotes: Quotes): Expr[JValue] =
    import quotes.reflect.{report, Position}
    import quotes.reflect.given
    val file = SourceFile(srcFilename.valueOrError, readFileString(relativeFilename))
    Parser(file).parseFile.fold(
      error => {
        import cats.syntax.all.catsSyntaxOrder
        val offset = error.expected.map(_.offset).toList.max
        val (line, col) = file.getLineCol(offset)
        report.throwError(
          s"syntax error at ${Console.UNDERLINED}${file.path}${Console.RESET}:$line:$col",
          Position.ofMacroExpansion,
        )
      },
      jvalue => Expr(jvalue),
    )

type JParamList = Seq[(String, Option[JValue])]
