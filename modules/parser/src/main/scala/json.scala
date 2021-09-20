package root

import cats.parse.{Parser0 => P0, Parser => P, Numbers}
import cats.syntax.all._

object Json {
  import JValue._
  val whitespace: P[Unit] = P.oneOf(List(
    P.charIn(" \t\r\n").void,
    P.char('#') *> P.charsWhile0(_ != '\n').void,
    (P.string("//") *> P.charsWhile0(_ != '\n').void).backtrack,
    (P.string("/*") *> P.oneOf(List(
      P.charWhere(_ != '*'),
      (P.char('*') *> !P.char('/')).backtrack,
    )).rep0 *> P.string("*/")),
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
  val listSep: P[Unit] = P.char(',').surroundedBy(__).void
  val dollar = P.char('$').map(_ => JOuter)

  def commaList[A](pa: P[A]): P0[List[A]] = (
    pa.<*(__) ~
    (P.char(',') *> (__) *> pa <* __).backtrack.rep0 <*
    P.char(',').<*(__).?
  ).map { (head, tail) =>
    head +: tail
  }.?.map(_.getOrElse(List.empty))

  val assert = P.defer {
    keyword("assert") *> expr.surroundedBy(__) ~ (P.char(':') *> __ *> expr).?
  }
  val objInside: P0[JValue] = P.defer0 {
    val bindLocal: P[JObjMember.JLocal] = (keyword("local") *> whitespaces *> bind).map { (id, paramsOpt, expr) =>
      JObjMember.JLocal(id, paramsOpt.fold(expr)(JFunction(_, expr)))
    }
    val objComp = (
      (bindLocal <* __ <* P.char(',') <* __).backtrack.rep0,
      __ *> P.char('[') *> __ *> expr <* __ <* P.char(']') <* __,
      P.char(':') *> __ *> expr <* __,
      (P.char(',') *> __ *> bindLocal <* __).backtrack.rep0,
      __ *> P.char(',').? *> __ *> forspec <* __,
      ifspec.?,
    ).tupled.map { case (preLocals, key, value, postLocals, (inId, inExpr), cond) =>
      JObjectComprehension(preLocals, key, value, postLocals, inId, inExpr, cond)
    }
    val key: P[JValue] = P.oneOf(List(
      string.map(JString(_)),
      id.map(id => JString(id)),
      (P.char('[') *> __ *> expr <* __ <* P.char(']')),
    ))
    val h = (P.char(':') ~ P.char(':').?  ~ P.char(':').?).map { case ((_1, _2), _3) =>
      _2.isDefined || _3.isDefined
    }
    val keyValue: P[JObjMember] = {
      P.oneOf(List(
        (key, h.surroundedBy(__), expr).tupled.map(JObjMember.JField(_, _, _)),
        assert.map(JObjMember.JAssert(_, _)),
        (keyword("local") *> __ *> bind).map { (id, paramsOpt, expr) =>
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
    P.char('(') *> commaList(id.<*(__) ~ (P.char('=') *> __ *> expr).?) <* P.char(')')
  }
  val bind: P[(String, Option[JParamList], JValue)] = P.defer {
    (
      id.<*(__),
      params.?.with1 <*(P.char('=').surroundedBy(__)),
      expr,
    ).tupled
  }
  val forspec = P.defer {
    (
      keyword("for") *> id.surroundedBy(__) <* keyword("in"),
      __.with1 *> expr,
    ).tupled
  }
  val ifspec = keyword("if") *> __ *> expr

  val exprBase: P[JValue] = P.defer {
    val listComp = (
      expr,
      forspec.surroundedBy(__) ~ ifspec.?,
    ).tupled.map { case (forExpr, ((forVar, inExpr), cond)) =>
      JArrayComprehension(forVar, forExpr, inExpr, cond)
    }
    val list =
      P.oneOf0(List(
        listComp.backtrack,
        commaList(expr).map(JArray(_)),
      ))
        .surroundedBy(__)
        .with1
        .between(P.char('['), P.char(']'))

    val function = (keyword("function") *> __ *> (params <* __) ~ expr).map(JFunction(_, _))
    val ifExpr = (
      (keyword("if") *> __ *> expr <* __) ~
      (keyword("then") *> __ *> expr <* __) ~
      (keyword("else") *> __ *> expr).?
    ).map { case ((cond, trueValue), elseValue) =>
      JIf(cond, trueValue, elseValue.getOrElse(JNull))
    }
    val obj = P.char('{') *> __ *> objInside <* __ <* P.char('}')
    val grouped = P.char('(') *> __ *> expr <* __ <* P.char(')')
    val local = (
      keyword("local") *> __ *> bind.repSep(__ *> P.char(',') *> __) ~
      (__ *> P.char(';') *> __ *> expr)
    ).map { (binds, result) =>
      binds.toList.foldRight(result) { case ((id, paramsOpt, expr), acc) =>
        JLocal(id, paramsOpt.fold(expr)(JFunction(_, expr)), acc)
      }
    }
    val error = (keyword("error") *> __ *> expr).map(JError(_))
    val importExpr = (keyword("import") *> __ *> string).map(JImport(_))
    val importStrExpr = (keyword("importstr") *> __ *> string).map(JImportStr(_))
    val assertExp = ((assert <* __ <* P.char(';') <* __) ~ expr).map { case ((cond, msg), result) =>
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
    val argName = (id <* __ <* P.char('=')).backtrack.?
    commaList(argName.<*(__).with1 ~ expr)
  }

  val unaryOp = P.charIn("+-!~")
  val exprAtom: P[JValue] = P.defer {
    val suffix: P[JValue => JValue] =
      P.charIn(Array('.', '(', '['))
        .<*(__)
        .flatMap {
          case '.' => id.map(field => JGetField(_, field))
          case '[' =>
            val exprOrJNull = expr.?.map(_.getOrElse(JNull))
            val suffix = P.char(':') *> __ *> exprOrJNull <* __
            (
              (expr <* __) ~ (suffix ~ suffix.?).? <* P.char(']')
            ).map { (index, opt) =>
              if opt.isEmpty then
                JIndex(_, index)
              else
                val (endIndex, strideOpt) = opt.get
                JSlice(_, index, endIndex, strideOpt.getOrElse(JNull))
            }
          case '(' => (args <* __).flatMap { args =>
            val namedAfterPositional = args.size >= 2 && args.sliding(2).exists { window =>
              val Seq((first, _), (second, _)) = window
              first.isDefined && second.isEmpty
            }
            if namedAfterPositional then
              P.failWith("Positional argument after a named argument is not allowed")
            else
              val (positional, named) = args.partition(_._1.isEmpty)
              P.pure(JApply(_, positional.map(_._2), named.map((id, expr) => id.get -> expr)))
          }.with1 <* P.char(')')
        }
        <* __
    ((unaryOp.? <* __).with1 ~ exprBase.<*(__) ~ suffix.rep0).map { case ((unaryOp, base), fns) =>
      val expr = fns.foldLeft(base) { (base, fn) => fn(base) }
      unaryOp match
        case None => expr
        case Some(op) => JUnaryOp(JUnaryOperator(op), expr)
    }
  }

  val expr = P.defer {
    (exprAtom.<*(__) ~ (op.surroundedBy(__) ~ exprAtom).rep0).map { (head, tail) =>
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
  val parserFile = __ *> expr <* __
}

@main
def asdf(args: String*): Unit =
  import cats.instances.all._
  //println((Json.id *> P.char('(') *> Json.exprAtom.surroundedBy(Json.__) <* Json.__ <* P.char(')')).parseAll(args(0)))
  val filename = args(0)
  val source = scala.io.Source.fromFile(filename).getLines.mkString("\n")
  val parser = Json.parserFile
  println("START")
  parser.parseAll(source).fold(
    error => {
      println("END")
      println("FAIL: " + error.toString)
    },
    ast => {
      println("END PARSE")
      val ctx = EvaluationContext()
      val evaluated = eval(ctx)(ast)
      val manifested = evaluated.manifest(ctx)
      println("END EVAL")
      manifested.fold(
        msg => println(s"FAIL: $msg"),
        value => println(value),
      )
    }
  )
