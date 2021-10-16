package root

import scala.quoted.*

opaque type Case[V] = Unit
object Case:
  def apply[T]: [V] => V => Case[V] =
    [V] => (e: V) => ()

object macros:
  def unfoldUnionType(
    quotes: Quotes,
  )(
    tpe: quotes.reflect.TypeRepr,
    list: collection.mutable.ArrayBuffer[quotes.reflect.TypeRepr] = collection.mutable.ArrayBuffer[quotes.reflect.TypeRepr](),
  ): collection.mutable.ArrayBuffer[quotes.reflect.TypeRepr] =
    tpe match
    case quotes.reflect.OrType(lhs, rhs) =>
      unfoldUnionType(quotes)(lhs, list)
      unfoldUnionType(quotes)(rhs, list)
    case other => list += other
    list

  transparent inline def typedUnionApply[T, V](scrutinee: Any, inline fn: Any => Any): V = ${ typedUnionApplyImpl[T, V]('scrutinee, 'fn) }
  def typedUnionApplyImpl[T: Type, V: Type](scrutinee: Expr[Any], casesExpr: Expr[Any => Any])(using quotes: Quotes): Expr[V] =
    import quotes.reflect._
    val cases = casesExpr.asTerm match
      case Inlined(_, _, NamedArg(_, Block(Seq(DefDef(_, _, _, Some(Match(_, cases)))), _))) => cases
      case Inlined(_, _, Block(Seq(DefDef(_, _, _, Some(Match(_, cases)))), _)) => cases
      case a =>
        report.throwError(s"extra cases must be a lambda with a top-level match statement\n${a}")
    var fallback: Option[CaseDef] = None
    val typeMapping = collection.mutable.ArrayBuffer[(TypeRepr, CaseDef)]()
    cases.foreach {
      // case caseDef @ CaseDef(Typed(_: Ident, typeTree), None, rhs) => typeMapping += (typeTree.tpe -> caseDef)
      case caseDef @ CaseDef(Ident("_"), None, _) => fallback = Some(caseDef)
      case caseDef @ CaseDef(Typed(id: Ident, typeTree), None, rhs) =>
        typeTree.tpe match
        case Applied(_, List(tpe1: TypeTree, tpe2: TypeTree)) =>
          typeMapping += (tpe1.tpe -> CaseDef(Typed(id, tpe2), None, rhs))
        case _ => 
          report.throwError(s"cases must be type of the form 'case _: (Type1, Type2) => expr'")
      case c =>
        report.throwError(s"cases must be type of the form 'case _: Type => expr'")
    }
    val typeMap = typeMapping.toMap
    val matchedCases = types.map { tpe =>
      typeMap.collectFirst {
        case (matchType, caseDef) if tpe <:< matchType => caseDef
      }.getOrElse(report.throwError(s"invalid type ${tpe.show}, expected one of: ${typeMap.map(_._1.show).mkString(", ")}"))
    }.toList
    val types = unfoldUnionType(quotes)(TypeRepr.of[T].dealias).toSeq
    Match(scrutinee.asTerm, fallback.fold(matchedCases)(matchedCases :+ _)).asExprOf[V]

  inline def mapUnionType[T, V](inline fn: Any => V): Seq[V] = ${ mapUnionTypeImp[T, V]('fn) }
  def mapUnionTypeImp[T: Type, V: Type](casesExpr: Expr[Any => V])(using quotes: Quotes): Expr[Seq[V]] =
    import quotes.reflect._
    val cases = casesExpr.asTerm match
      case Inlined(_, _, NamedArg(_, Block(Seq(DefDef(_, _, _, Some(Match(_, cases)))), _))) => cases
      case Inlined(_, _, Block(Seq(DefDef(_, _, _, Some(Match(_, cases)))), _)) => cases
      case a =>
        report.throwError(s"extra cases must be a lambda with a top-level match statement\n${a}")
    val typeMap = cases.map {
      case CaseDef(Typed(Ident("_"), typeTree), None, rhs) => typeTree.tpe -> rhs
      case c =>
        report.throwError(s"cases must be type of the form 'case _: Type => expr'")
    }
    val types = unfoldUnionType(quotes)(TypeRepr.of[T].dealias).toSeq
    val applications = Varargs(types.map { tpe =>
      typeMap.collectFirst {
        case (matchType, value) if tpe <:< matchType =>
          value.asExprOf[V]
      }.getOrElse(report.throwError(s"invalid type ${tpe.show}, expected one of: ${typeMap.map(_._1.show).mkString(", ")}"))
    })
    applications

  inline def typeString[T]: String = ${typeStringImp[T]}
  def typeStringImp[T: Type](using quotes: Quotes): Expr[String] =
    import quotes.reflect._
    Expr(Type.show[T])

  inline def typeTree[T]: String = ${typeTreeImp[T]}
  def typeTreeImp[T: Type](using quotes: Quotes): Expr[String] =
    import quotes.reflect._
    Expr(TypeRepr.of[T].toString)

  inline def exprTree(inline expr: Any): String = ${exprTreeImp('expr)}
  def exprTreeImp(expr: Expr[Any])(using quotes: Quotes): Expr[String] =
    import quotes.reflect._
    Expr(expr.asTerm.toString)

  inline def log(inline msg: String): String = ${logImp('msg)}
  def logImp(msgExpr: Expr[String])(using quotes: Quotes): Expr[String] =
    val msg = msgExpr.value
    if msg.isEmpty then quotes.reflect.report.error("not a contstant string literal")
    quotes.reflect.report.info(msg.get)
    msgExpr
