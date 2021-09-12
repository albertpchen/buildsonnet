package root

import cats.parse.{Parser0 => P0, Parser => P, Numbers}
import cats.syntax.all._

object Json {
  import JValue._
  val commentEnd: P0[Unit] = P.oneOf0(List(
    P.char('#') *> P.charsWhile0(_ != '\n') *> P.end,
    P.string("//") *> P.charsWhile0(_ != '\n') *> P.end,
    P.end,
  ))
  val whitespace: P[Unit] = P.oneOf(List(
    P.charIn(" \t\r\n").void,
    P.char('#') *> P.charsWhile0(_ != '\n') *> P.char('\n').withContext("SDLFJLDJFKLJ"),
    P.string("//") *> P.charsWhile0(_ != '\n') *> P.char('\n').withContext("ikk"),
    (P.string("/*") *> P.oneOf(List(
      P.charWhere(_ != '*'),
      (P.char('*') *> !P.char('/')).backtrack,
    )).rep0 *> P.string("*/")).backtrack,
  ))
  val whitespaces0, __ = whitespace.rep0.void
  val whitespaces: P[Unit] = whitespace.rep.void

  val idTail = P.charIn('_' +: ('a' to 'z') ++: ('A' to 'Z') ++: ('0' to '9'))
  def keyword(str: String): P[Unit] = P.string(str).void <* !idTail
    val keywords = Set(
    "assert", "else", "error", "false", "for", "function", "if", "import", "importstr",
    "in", "local", "null", "tailstrict", "then", "self", "super", "true"
  )
  val id: P[String] =
    P.not(P.stringIn(keywords) *> !idTail)
      .with1
      .*>(P.charIn('_' +: ('a' to 'z') ++: ('A' to 'Z')) ~ idTail.rep0).map { (head, tail) =>
        (head +: tail).mkString
      }

  val pnull = keyword("null").as(JNull)
  val ptrue = keyword("true").as(JTrue)
  val pfalse = keyword("false").as(JTrue)
  val psuper = keyword("super").map(_ => JSuper)
  val self = keyword("self").map(_ => JSelf)

  val hexDigit = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).map { ch =>
    if ch >= '0' || ch <= '9' then
      ch - '0'
    else if ch >= 'a' || ch <= 'f' then
      ch - 'a' + 10
    else
      ch - 'A' + 10
  }

  val escapedStringChar: P[Char] = P.char('\\') *> P.oneOf(List(
    P.char('"').map(_ => '"'),
    P.char('\'').map(_ => '\''),
    P.char('\\').map(_ => '\\'),
    P.char('/').map(_ => '/'),
    P.char('b').map(_ => '\b'),
    P.char('f').map(_ => '\f'),
    P.char('n').map(_ => '\n'),
    P.char('r').map(_ => '\r'),
    P.char('t').map(_ => '\t'),
    P.char('u') *> (hexDigit, hexDigit, hexDigit, hexDigit).tupled.map { (_1, _2, _3, _4) =>
      (_1.toInt << 12 | _2.toInt << 8 | _3.toInt << 4 | _4.toInt).toChar
    },
  ))
  def stringChar(quote: Char): P[Char] = P.oneOf(List(
    P.charWhere(c => c >= ' ' && c != quote && c != '\\'),
    escapedStringChar,
  ))
  def verbatimStringChar(quote: Char): P[Char] = P.oneOf(List(
    P.charWhere(c => c >= ' ' && c != quote),
    P.char(quote) *> P.char(quote).map(_ => quote),
  ))

  val string = P.oneOf(List(
    P.char('\'') *> stringChar('\'').rep0 <* P.char('\''),
    P.char('"') *> stringChar('"').rep0 <* P.char('"'),
    P.string("@'") *> verbatimStringChar('\'').backtrack.rep0 <* P.char('\''),
    P.string("@\"") *> verbatimStringChar('"').backtrack.rep0 <* P.char('"'),
  )).map(s => s.mkString)

  val num = Numbers.jsonNumber.map(JNum(_))
  val listSep: P[Unit] = P.char(',').surroundedBy(whitespaces0).void
  val dollar = P.char('$').map(_ => JOuter)

  def commaList[A](pa: P[A]): P0[List[A]] = (
    pa.<*(whitespaces0) ~
    (P.char(',') *> (whitespaces0) *> pa <* whitespaces0).backtrack.rep0 <*
    P.char(',').<*(whitespaces0).?
  ).map { (head, tail) =>
    head +: tail
  }.?.map(_.getOrElse(List.empty))

  val assert = P.defer {
    keyword("assert") *> expr.surroundedBy(whitespaces0) ~ (P.char(':') *> whitespaces0 *> expr).?
  }
  val objInside: P0[JValue] = P.defer0 {
    val objComp = {
      val bindLocal: P[JObjMember.JLocal] = (keyword("local") *> whitespaces *> bind).map { (id, paramsOpt, expr) =>
        JObjMember.JLocal(id, paramsOpt.fold(expr)(JFunction(_, expr)))
      }
      (
        (bindLocal <* P.char(',').surroundedBy(whitespaces0)).backtrack.rep0,
        expr.surroundedBy(whitespaces0).between(P.char('['), P.char(']')).surroundedBy(whitespaces0),
        P.char(':') *> expr.surroundedBy(whitespaces0),
        (P.char(',') *> bindLocal.surroundedBy(whitespaces0)).backtrack.rep0,
        whitespaces0 *> P.char(',').? *> forspec.surroundedBy(whitespaces0),
        ifspec.?,
      ).tupled.map { case (preLocals, key, value, postLocals, (inId, inExpr), cond) =>
        JObjectComprehension(preLocals, key, value, postLocals, inId, inExpr, cond)
      }
    }
    val key: P[JValue] = P.oneOf(List(
      string.map(JString(_)),
      id.map(id => JString(id)),
      (P.char('[') *> whitespaces0 *> expr <* whitespaces0 <* P.char(']')),
    ))
    val h = (P.char(':') ~ P.char(':').?  ~ P.char(':').?).map { case ((_1, _2), _3) =>
      _2.isDefined || _3.isDefined
    }
    val keyValue: P[JObjMember] = {
      P.oneOf(List(
        (key, h.surroundedBy(whitespaces0), expr).tupled.map(JObjMember.JField(_, _, _)),
        assert.map(JObjMember.JAssert(_, _)),
        (keyword("local") *> whitespaces0 *> bind).map { (id, paramsOpt, expr) =>
          JObjMember.JLocal(id, paramsOpt.fold(expr)(JFunction(_, expr)))
        }.backtrack,
      ))
    }
    P.oneOf0(List(
      objComp.backtrack,
      commaList(keyValue).map(JValue.JObject(_)),
    ))
  }
  val params: P0[List[(String, Option[JValue])]] = P.defer0 {
    P.char('(') *> commaList(id.<*(whitespaces0) ~ (P.char('=') *> whitespaces0 *> expr).?) <* P.char(')')
  }
  val bind: P[(String, Option[JParamList], JValue)] = P.defer {
    (
      id.<*(whitespaces0),
      params.?.with1 <*(P.char('=').surroundedBy(whitespaces0)),
      expr,
    ).tupled
  }
  val forspec = P.defer {
    (
      keyword("for") *> id.surroundedBy(whitespaces0) <* keyword("in"),
      whitespaces0.with1 *> expr,
    ).tupled
  }
  val ifspec = keyword("if") *> whitespaces0 *> expr

  val exprBase: P[JValue] = P.defer {
    val listComp = (
      expr,
      forspec.surroundedBy(whitespaces0) ~ ifspec.?,
    ).tupled.map { case (forExpr, ((forVar, inExpr), cond)) =>
      JArrayComprehension(forVar, forExpr, inExpr, cond)
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

    val grouped =
      expr
        .surroundedBy(whitespaces0)
        .between(P.char('('), P.char(')'))

    val local = (
      keyword("local") *> whitespaces0 *> bind.repSep(P.char(',').surroundedBy(whitespaces0)) ~
      (P.char(';').surroundedBy(whitespaces0) *> expr)
    ).map { (binds, result) =>
      binds.toList.foldRight(result) { case ((id, paramsOpt, expr), acc) =>
        JLocal(id, paramsOpt.fold(expr)(JFunction(_, expr)), acc)
      }
    }

    val error = (keyword("error") *> whitespaces0 *> expr).map(JError(_))
    val importExpr = (keyword("import") *> whitespaces0 *> string).map(JImport(_))
    val importStrExpr = (keyword("importstr") *> whitespaces0 *> string).map(JImportStr(_))
    val assertExp = (assert.<*(P.char(';').surroundedBy(whitespaces0)) ~ expr).map { case ((cond, msg), result) =>
      JAssert(cond, msg, result)
    }

    P.oneOf(List(
      string.map(JString(_)),
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
      id.map(JId(_)),
      error,
    ))
  }

  val op = P.stringIn(Array(
    "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
    "*", "/", "%", "+", "-", "<", ">", "&", "^", "|", "in",
  )).map(JBinaryOperator(_))

  val args: P0[List[(Option[String], JValue)]] = P.defer0 {
    val argName = (id <* whitespaces0 <* P.char('=')).backtrack.?
    commaList(argName.<*(whitespaces0).with1 ~ expr)
  }

  val unaryOp = P.charIn("+-!~")
  val exprAtom: P[JValue] = P.defer {
    val suffix: P[JValue => JValue] =
      P.charIn(Array('.', '(', '['))
        .<*(whitespaces0)
        .flatMap {
          case '.' => id.map(field => JGetField(_, field))
          case '[' => (expr <* whitespaces0 <* P.char(']')).map(idx => JIndex(_, idx))
          case '(' => args.<*(whitespaces0).flatMap { args =>
            val namedAfterPositional = args.size >= 2 && args.sliding(2).exists { window =>
              val first = window(0)._1
              val second = window(1)._1
              first.isDefined && second.isEmpty
            }
            if namedAfterPositional then
              P.failWith("Positional argument after a named argument is not allowed")
            else
              val (positional, named) = args.partition(_._1.isEmpty)
              P.pure(JApply(_, positional.map(_._2), named.map((id, expr) => id.get -> expr)))
          }.with1 <* P.char(')')
        }
        <* whitespaces0
    ((unaryOp.? <* whitespaces0).with1 ~ exprBase.<*(whitespaces0) ~ suffix.rep0).map { case ((unaryOp, base), fns) =>
      val expr = fns.foldLeft(base) { (base, fn) => fn(base) }
      unaryOp match
        case None => expr
        case Some(op) => JUnaryOp(JUnaryOperator(op), expr)
    }
  }

  val expr = P.defer {
    (exprAtom.<*(whitespaces0) ~ (op.surroundedBy(whitespaces0) ~ exprAtom).rep0).map { case (head, tail) =>
      var exprs = tail
      def climb(curr: JValue, minPrec: Int): JValue =
        var result = curr
        while
          if exprs.isEmpty then
            false
          else
            val (op, expr) = exprs.head
            val cond = exprs.head._1.precedence >= minPrec
            if cond then
              val nextPrec = if op.isLeftAssociative then minPrec + 1 else minPrec
              exprs = exprs.tail
              val rhs = climb(expr, nextPrec)
              result = JBinaryOp(result, op, rhs)
            cond
        do ()
        result
      climb(head, 0)
    }
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile = expr.between(whitespaces0, whitespace.backtrack.rep0 ~ commentEnd)
  // val parserFile = whitespace.backtrack.rep0 ~ commentEnd//expr.between(whitespaces0, whitespaces0 ~ commentEnd)
}

@main
def asdf(args: String*): Unit =
  import cats.instances.all._
  //println((Json.id *> P.char('(') *> Json.exprAtom.surroundedBy(Json.whitespaces0) <* Json.whitespaces0 <* P.char(')')).parseAll(args(0)))
  val filename = args(0)
  val source = scala.io.Source.fromFile(filename).getLines.mkString("\n")
  println("START")
  println(Json.parserFile.parseAll(source).fold(_.expected.map(_.toString).mkString_("\n"), _.toString))
  println("END")
  println("START")
  println(Json.parserFile.parseAll(args(1)).fold(_.expected.map(_.toString).mkString_("\n"), _.toString))
  println("END")
