package root

import cats.parse.{Parser0 => P0, Parser => P, Numbers}
import cats.syntax.all._

enum JObjInside:
  case JObjMembers(members: Seq[JObjMember])
  case JObjComprehension(
    preLocals: Seq[JObjMember.JLocal],
    comp: JObjMember.JField,
    postLocals: Seq[JObjMember.JLocal],
    inExprs: Seq[JValue],
    cond: Option[JValue],
  )

enum JObjMember:
  case JLocal(name: String, value: JValue)
  case JField(key: JValue, value: JValue)
  case JAssert(cond: JValue, expr: Option[JValue])

enum JValue:
  case JString(str: String)
  case JFalse
  case JTrue
  case JNull
  case JSelf
  case JSuper
  case JOuter
  case JNum(str: String)
  case JArray(elements: Seq[JValue])
  case JObject(inside: JObjInside)
  case JId(name: String)
  case JGetField(loc: JValue, field: JId)
  case JIndex(loc: JValue, index: JValue)
  case JApply(loc: JValue, args: Seq[(Option[JId], JValue)])
  case JBinaryOp(left: JValue, op: String, right: JValue)
  case JLocal(name: String, value: JValue, result: JValue)
  case JArrComprehension(comp: JValue, inExprs: Seq[JValue], cond: Option[JValue])
  case JFunction(params: JParamList, body: JValue)
  case JIf(cond: JValue, trueValue: JValue, elseValue: JValue)
  case JError(expr: JValue)
  case JAssert(cond: JValue, expr: Option[JValue], result: JValue)
  case JImport(file: String)
  case JImportStr(file: String)
  case JArrayComprehension(
    forVar: String,
    forExpr: JValue,
    inExpr: JValue,
    cond: Option[JValue],
  )

type JParamList = Seq[(JValue.JId, Option[JValue])]


object Json {
  import JValue._
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val whitespaces0, __ = whitespace.rep0.void
  val whitespaces: P[Unit] = whitespace.rep.void

  val idTail = P.charIn('_' +: ('a' to 'z') ++: ('A' to 'Z') ++: ('0' to '9'))
  def keyword(str: String): P[Unit] = P.string(str).void <* !idTail
  val keywords = Set(
    "assert", "else", "error", "false", "for", "function", "if", "import", "importstr",
    "in", "local", "null", "tailstrict", "then", "self", "super", "true"
  )
  val id: P[JId] =
    P.not(P.stringIn(keywords) *> !idTail)
      .with1
      .*>(P.charIn('_' +: ('a' to 'z') ++: ('A' to 'Z')) ~ idTail.rep0).map { (head, tail) =>
        JId((head +: tail).mkString)
      }

  val pnull = keyword("null").as(JNull)
  val ptrue = keyword("true").as(JTrue)
  val pfalse = keyword("false").as(JTrue)
  val psuper = keyword("super").map(_ => JSuper)
  val self = keyword("self").map(_ => JSelf)
  val justStr = JsonStringUtil.escapedString('"')
  val str = justStr.map(JString(_))
  val num = Numbers.jsonNumber.map(JNum(_))
  val listSep: P[Unit] = P.char(',').surroundedBy(whitespaces0).void
  val dollar = P.char('$').map(_ => JOuter)

  def commaList[A](pa: P[A]): P0[List[A]] = (
    pa.<*(whitespaces0) ~
    (P.char(',').surroundedBy(whitespaces0) *> pa).surroundedBy(whitespaces0).backtrack.rep0 <*
    P.char(',').<*(whitespaces0).?
  ).map { (head, tail) =>
    head +: tail
  }.?.map(_.getOrElse(List.empty))

  val assert = P.defer {
    keyword("assert") *> expr.surroundedBy(whitespaces0) ~ (P.char(':') *> whitespaces0 *> expr).?
  }
  val objInside: P0[JObjInside] = P.defer0 {
    val key: P[JValue] = P.oneOf(List(
      justStr.map(JString(_)),
      id.map(id => JString(id.name)),
      (P.char('[') *> whitespaces0 *> expr <* whitespaces0 <* P.char(']')),
    ))
    val keyValue: P[JObjMember] = {
      import JObjMember._
      P.oneOf(List(
        (key ~ (P.char(':').surroundedBy(whitespaces0) *> expr)).map(JField(_, _)),
        assert.map(JAssert(_, _)),
        (keyword("local") *> whitespaces0 *> bind).map { (id, paramsOpt, expr) =>
          JLocal(id.name, paramsOpt.fold(expr)(JFunction(_, expr)))
        },
      ))
    }
    commaList(keyValue).map(JObjInside.JObjMembers(_))
  }
  val params: P0[List[(JId, Option[JValue])]] = P.defer0 {
    P.char('(') *> commaList(id.<*(whitespaces0) ~ (P.char('=') *> whitespaces0 *> expr).backtrack.?) <* P.char(')')
  }
  val bind: P[(JId, Option[JParamList], JValue)] = P.defer {
    (
      id.<*(whitespaces0),
      params.?.with1 <*(P.char('=').surroundedBy(whitespaces0)),
      expr,
    ).tupled
  }
  val forspec = (
    keyword("for") *> whitespaces0 *> id <* keyword("in").surroundedBy(whitespaces0),
    expr
  ).tupled
  val ifspec = keyword("if") *> whitespaces0 *> expr
  val objComp = P.defer0 {
    import JObjMember._
    val bindPrefix =
      keyword("local")
        *> whitespaces0
        *> id
        <* whitespaces0
        <* P.char('=')
        <* whitespaces0
    val bindLocal = bind.map { (id, paramsOpt, expr) =>
      JLocal(id.name, paramsOpt.fold(expr)(JFunction(_, expr)))
    }
    (
      (bindLocal <* P.char(',').surroundedBy(whitespaces0)).backtrack.rep0,
      expr.surroundedBy(whitespaces0).between(P.char('['), P.char(']')).surroundedBy(whitespaces0),
      P.char(':') *> expr.surroundedBy(whitespaces0),
      (P.char(',') *> bindLocal.surroundedBy(whitespaces0)).rep0,
      forspec <* whitespaces0,
      ifspec.?,
    ).tupled
  }

  val exprBase: P[JValue] = P.defer {
    val listComp = (
      expr <* keyword("for").surroundedBy(whitespaces0),
      id <* keyword("in").surroundedBy(whitespaces0),
      expr.<*(whitespaces0) ~ (keyword("if") *> whitespaces0 *> expr).?,
    ).tupled.map { case (forExpr, forVar, (inExpr, cond)) =>
      JArrayComprehension(forVar.name, forExpr, inExpr, cond)
    }
    val list =
      P.oneOf0(List(
        listComp.backtrack,
        commaList(expr).map(JArray(_)),
      ))
        .surroundedBy(whitespaces0)
        .with1
        .between(P.char('['), P.char(']'))

    val function =
      (keyword("function") *> params.surroundedBy(whitespaces0) ~ expr).map { (params, body) =>
        JFunction(params, body)
      }
    val ifExpr = (keyword("if") *> whitespaces0 *> expr.<*(keyword("then")) ~ expr ~ (keyword("else") *> expr).?).map { case ((cond, trueValue), elseValue) =>
        JIf(cond, trueValue, elseValue.getOrElse(JNull))
      }

    val obj =
      objInside
        .surroundedBy(whitespaces0)
        .with1
        .between(P.char('{'), P.char('}'))
        .map(JObject(_))

    val grouped =
      expr
        .surroundedBy(whitespaces0)
        .between(P.char('('), P.char(')'))

    val local = (
      keyword("local") *> bind.repSep(P.char(',').surroundedBy(whitespaces0)),
      P.char('=').surroundedBy(whitespaces0) *> expr,
      P.char(';').surroundedBy(whitespaces0) *> expr,
    ).tupled.map { (binds, expr, result) =>
      binds.toList.foldRight(result) { case ((id, paramsOpt, expr), acc) =>
        JLocal(id.name, paramsOpt.fold(expr)(JFunction(_, expr)), acc)
      }
    }

    val error = (keyword("error") *> whitespaces0 *> expr).map(JError(_))
    val importExpr = (keyword("import") *> whitespaces0 *> justStr).map(JImport(_))
    val importStrExpr = (keyword("importstr") *> whitespaces0 *> justStr).map(JImportStr(_))
    val assertExp = (assert.<*(P.char(';').surroundedBy(whitespaces0)) ~ expr).map { case ((cond, msg), result) =>
      JAssert(cond, msg, result)
    }

    P.oneOf(List(
      str,
      num,
      list,
      obj,
      grouped,
      pnull.backtrack,
      pfalse.backtrack,
      ptrue.backtrack,
      self.backtrack,
      psuper.backtrack,
      dollar,
      ifExpr,
      function.backtrack,
      local.backtrack,
      assertExp.backtrack,
      importExpr.backtrack,
      importStrExpr.backtrack,
      id,
      error,
    ))
  }

  val op = P.stringIn(Array(
    "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
    "*", "/", "%", "+", "-", "<", ">", "&", "^", "|", "in",
  ))
  val args: P0[List[(Option[JId], JValue)]] = P.defer0 {
    val argName = (id <* whitespaces0 <* P.char('=')).backtrack.?
    commaList(argName.<*(whitespaces0).with1 ~ expr)
  }

  val exprAtom: P[JValue] = P.defer {
    val suffix: P[JValue => JValue] =
      P.charIn(Array('.', '(', '['))
        .surroundedBy(whitespaces0)
        .flatMap {
          case '.' => id.map(field => JGetField(_, field))
          case '[' => (expr <* whitespaces0 <* P.char(']')).map(idx => JIndex(_, idx))
          case '(' => args.<*(whitespaces0).map(args => JApply(_, args)).with1 <* P.char(')')
        }
        <* whitespaces0
    (exprBase ~ suffix.backtrack.rep0).map { (base, fns) =>
      fns.foldLeft(base) { (base, fn) => fn(base) }
    }
  }

  val expr = P.defer {
    // def prec(ch: Char): Int = ???
    // def isLeftAssoc(ch: Char): Bool = ???
    // var result = exprAtom()
    // def climb(min_prec: Int, expr) =
    //   P.charIn(Seq('*', '+', '-')).flatMap { op =>
    //     val prec = prec(op)
    //     while prec >= min_prec do
    //       val nextMinPrec = if isLeftAssoc(ch) then
    //         nextMinPrec = min_prec + 1
    //       else
    //         nextMinPrec = prec
    //       val rhs = climb(nextMinPrec)
    //       s
    //   }
    (exprAtom.<*(whitespaces0) ~ (op.surroundedBy(whitespaces0) ~ exprAtom).rep0).map { case (head, tail) =>
      tail.foldLeft(head) { case (left, (op, right)) =>
        JBinaryOp(left, op, right)
      }
    }
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P[JValue] = expr.between(whitespaces0, whitespaces0 ~ P.end)
}

object JsonStringUtil extends GenericStringUtil {
  // Here are the rules for escaping in json
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar), // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )
}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

  val escapedToken: P[Unit] = {
    val escapes = P.charIn(decodeTable.keys.toSeq)

    val oct = P.charIn('0' to '7')
    val octP = P.char('o') ~ oct ~ oct

    val hex = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val hex2 = hex ~ hex
    val hexP = P.char('x') ~ hex2

    val hex4 = hex2 ~ hex2
    val u4 = P.char('u') ~ hex4
    val hex8 = hex4 ~ hex4
    val u8 = P.char('U') ~ hex8

    val after = P.oneOf[Any](escapes :: octP :: hexP :: u4 :: u8 :: Nil)
    (P.char('\\') ~ after).void
  }

  /** String content without the delimiter
    */
  def undelimitedString(endP: P[Unit]): P[String] =
    escapedToken.backtrack
      .orElse((!endP).with1 ~ P.anyChar)
      .rep
      .string
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => P.pure(str1)
          case Left(_) => P.fail
        }
      }

  private val simpleString: P0[String] =
    P.charsWhile0(c => c >= ' ' && c != '"' && c != '\\')

  def escapedString(q: Char): P[String] = {
    val end: P[Unit] = P.char(q)
    end *> ((simpleString <* end).backtrack
      .orElse(undelimitedString(end) <* end))
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape = if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else
        encodeTable.get(c) match {
          case None =>
            if (c < ' ') nonPrintEscape(c.toInt)
            else c.toString
          case Some(esc) => esc
        }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~(str.length)
    }
    @annotation.tailrec
    def loop(idx: Int): Int =
      if (idx >= str.length) {
        // done
        idx
      } else if (idx < 0) {
        // error from decodeNum
        idx
      } else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        } else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          } else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o' => loop(decodeNum(idx + 2, 2, 8))
                  case 'x' => loop(decodeNum(idx + 2, 2, 16))
                  case 'u' => loop(decodeNum(idx + 2, 4, 16))
                  case 'U' => loop(decodeNum(idx + 2, 8, 16))
                  case other =>
                    // \c is interpretted as just \c, if the character isn't escaped
                    sb.append('\\')
                    sb.append(other)
                    loop(idx + 2)
                }
            }
          }
        }
      }

    val res = loop(0)
    if (res < 0) Left(~res)
    else Right(sb.toString)
  }
}

@main
def asdf(args: String*): Unit =
  //println((Json.id *> P.char('(') *> Json.exprAtom.surroundedBy(Json.whitespaces0) <* Json.whitespaces0 <* P.char(')')).parseAll(args(0)))
  println(Json.parserFile.parseAll(args(0)))
