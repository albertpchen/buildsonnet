package buildsonnet.evaluator

import cats.{Applicative, Eval, Monad, Monoid, Foldable, Parallel, Traverse}
import cats.data.Chain
import cats.effect.{Async, Concurrent, Sync, Ref}
import cats.syntax.all.given
import cats.instances.all.given

import buildsonnet.ast.*
import buildsonnet.logger.ConsoleLogger
import cats.syntax.ParallelTraversableOps1

given Traverse[Iterable] with
  override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

  override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable.iterateRight(fa, lb)(f)

  override def foldMap[A, B](fa: Iterable[A])(f: A => B)(implicit B: Monoid[B]): B =
    B.combineAll(fa.iterator.map(f))

  override def reduceLeftOption[A](fa: Iterable[A])(f: (A, A) => A): Option[A] = fa.reduceLeftOption(f)

  override def collectFirst[A, B](fa: Iterable[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

  override def fold[A](fa: Iterable[A])(implicit A: Monoid[A]): A = fa.fold(A.empty)(A.combine)

  override def find[A](fa: Iterable[A])(f: A => Boolean): Option[A] = fa.find(f)

  override def toIterable[A](fa: Iterable[A]): Iterable[A] = fa

  override def exists[A](fa: Iterable[A])(p: A => Boolean): Boolean = fa.exists(p)

  override def forall[A](fa: Iterable[A])(p: A => Boolean): Boolean = fa.forall(p)

  override def toList[A](fa: Iterable[A]): List[A] = fa.toList

  override def isEmpty[A](fa: Iterable[A]): Boolean = fa.isEmpty

  override def nonEmpty[A](fa: Iterable[A]): Boolean = fa.nonEmpty

  import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq

  private def toImIndexedSeq[A](fa: Iterable[A]): IndexedSeq[A] = fa match {
    case iseq: IndexedSeq[A] => iseq
    case _ =>
      val as = collection.mutable.ArrayBuffer[A]()
      as ++= fa
      wrapMutableIndexedSeq(as)
  }

  // Adapted from List and Vector instances.
  override def traverse[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] =
    if (fa.isEmpty) G.pure(Iterable.empty)
    else G.map(Chain.traverseViaChain(toImIndexedSeq(fa))(f))(_.toVector)


def eval[F[_]: Async: ConsoleLogger: Parallel](
  ctx: EvaluationContext[F],
)(jvalue: JValue): F[EvaluatedJValue[F]] =
  jvalue match
  case JValue.JFalse(src) => EvaluatedJValue.JBoolean(src, false).pure
  case JValue.JTrue(src) => EvaluatedJValue.JBoolean(src, true).pure
  case JValue.JNull(src) => EvaluatedJValue.JNull(src).pure
  case JValue.JSelf(src) => ctx.self.fold(ctx.error(src, "no self"))(_.pure)
  case JValue.JSuper(src) => ctx.`super`.fold(ctx.error(src, "no super"))(_.pure)
  case JValue.JOuter(src) =>
    ctx
      .lookup("$")
      .fold(ctx.error(src, s"no variable $$ defined"))(_.value)
  case JValue.JString(src, str) => EvaluatedJValue.JString(src, str).pure
  case JValue.JNum(src, str) => EvaluatedJValue.JNum(src, str.toDouble).pure
  case JValue.JArray(src, elements) =>
    for
      res <- Sync[F].delay(Array.ofDim[EvaluatedJValue[F]](elements.size))
      _ <- ((0 until elements.size): Iterable[Int]).parTraverse { i =>
        eval(ctx)(elements(i)).flatMap { e =>
          Sync[F].delay(res(i) = e)
        }
      }
    yield
      EvaluatedJValue.JArray(src, IArray.unsafeFromArray(res))
  case JValue.JLocal(_, name, value, result) =>
    ctx.bindCode(name, value).flatMap(eval(_)(result))
  case JValue.JId(src, name) =>
    ctx
      .lookup(name)
      .fold(ctx.error(src, s"no variable $name defined"))(_.value)
  case JValue.JGetField(src, loc, field) =>
    for
      loc <- eval(ctx)(loc)
      obj <- ctx.expect[EvaluatedJValue.JObject[F]](loc)
      value <- obj
        .members
        .get(field)
        .fold(ctx.error(src, s"object does not have field $field")) { value =>
          value.value
        }
    yield
      value
  case JValue.JObject(src, rawMembers) =>
    val pairs = rawMembers.collect {
      case JObjMember.JField(src, rawKey, plus, isHidden, rawValue) =>
        for
          rawKey <- eval(ctx)(rawKey)
          key <- ctx.expectFieldName(rawKey)
        yield
          key match
          case _: EvaluatedJValue.JNull[F] => None
          case expr: EvaluatedJValue.JString[F] =>
            val key = expr.string
            val value =
              if plus then
                JValue.JBinaryOp(
                  src,
                  JValue.JGetField(src, JValue.JSuper(src), key),
                  JBinaryOperator.Op_+,
                  rawValue
                )
              else
                rawValue
            Some((key, isHidden, value))
    }.parSequence
    def membersFn(ctx: EvaluationContext[F]) =
      val asserts = rawMembers.collect {
        case JObjMember.JAssert(src, rawCond, rawMsg) =>
          val cond = eval(ctx)(rawCond).flatMap(ctx.expect[Boolean](_))
          val msgOpt = rawMsg.fold(Option.empty[String].pure) { msg =>
            eval(ctx)(msg).flatMap(ctx.expect[String](_).map(Some(_)))
          }
          (cond, msgOpt).parTupled.flatMap { (cond, msgOpt) =>
            if cond then
              ().pure
            else
              ctx.error[Unit](src, msgOpt.getOrElse("object assertion failed"))
          }
      }.sequence
      val impl =
        for
          ctx <- ctx.bindCode(rawMembers.collect {
            case local: JObjMember.JLocal => local.name -> local.value
          })
          members <- pairs.flatMap(_.collect { case Some((key, isHidden, value)) =>
            LazyObjectValue[F](isHidden, eval(ctx)(value)).map(key -> _)
          }.sequence)
        yield
          collection.Map.from(members)
      EvaluatedJValue.JObjectImpl[F](impl, asserts.void)

    EvaluatedJValue.JObject(jvalue.src, ctx, membersFn).widen

  case JValue.JObjectComprehension(src, locals, rawKey, value, forVar, inExpr, condOpt) =>
    val pairs = for
      jvalue <- eval(ctx)(inExpr)
      arr <- ctx.expect[EvaluatedJValue.JArray[F]](jvalue)
      pairs <- arr.elements.map { elem =>
        for
          rawKey <- eval(ctx.bindStrict(forVar, elem))(rawKey)
          key <- ctx.expectFieldName(rawKey)
        yield
          key match
          case _: EvaluatedJValue.JNull[F] => None
          case key: EvaluatedJValue.JString[F] => Some((key.string, value, elem))
      }.toList.sequence
    yield
      pairs.flatten

    def membersFn(ctx: EvaluationContext[F]) =
      val impl = for
        ctx <- ctx.bindCode(locals.map(local => local.name -> local.value))
        members <- pairs.flatMap(_.map { (key, value, forValue) =>
          val valueCtx = ctx.bindStrict(forVar, forValue)
          LazyObjectValue(false, eval(ctx)(value)).map(key -> _)
        }.sequence)
      yield
        collection.Map.from(members)
      EvaluatedJValue.JObjectImpl[F](impl, ().pure)
    EvaluatedJValue.JObject(jvalue.src, ctx, membersFn).widen

  case JValue.JIndex(src, loc, rawIndex) =>
    ctx.expect[EvaluatedJValue.JArray[F] | EvaluatedJValue.JObject[F]](loc).flatMap {
      case obj: EvaluatedJValue.JObject[F] =>
        ctx.expect[String](rawIndex).flatMap { field =>
          obj.lookup(src, field).flatMap(_.value)
        }
      case arr: EvaluatedJValue.JArray[F] =>
        ctx.expect[Double](rawIndex).flatMap { num =>
          if num.isValidInt then
            val idx = num.toInt
            if idx >= arr.elements.size then
              ctx.error(src, s"index $idx out of bounds for length ${arr.elements.size}")
            else
              arr.elements(idx).pure
          else
            ctx.error(src, s"array index was not integer: $num")
        }
    }

  case JValue.JSlice(src, loc, rawIndex, rawEndIndex, rawStride) =>
    ctx.expect[EvaluatedJValue.JArray[F] | EvaluatedJValue.JObject[F]](loc).flatMap {
      case obj: EvaluatedJValue.JObject[F] =>
        ctx.error(src, "no end index or stride allowed for object index")
      case arr: EvaluatedJValue.JArray[F] =>
        val none = Option.empty[Int].pure
        (
          ctx.expect[Double](rawIndex).flatMap { rawIdx =>
            if rawIdx.isValidInt then
              rawIdx.toInt.pure
            else
              ctx.error(src, s"array index was not integer: $rawIdx")
          },
          rawEndIndex.fold(none)(num => ctx.expect[Double](num).map(n => Some(n.toInt))),
          rawStride.fold(none)(num => ctx.expect[Double](num).map(n => Some(n.toInt))),
        ).parMapN { (idx, endIdxOpt, strideOpt) =>
          //arr.slice(src, ctx, index.double.toInt, endIndex, stride)
          val endIdx = endIdxOpt.getOrElse(arr.elements.size)
          val stride = strideOpt.getOrElse(1)
          for
            _ <- if idx < 0 || endIdx < 0 || stride < 0 then
              ctx.error(src, s"negative index, end, or stride are not allowed")
            else
              ().pure
            size = arr.elements.size
            _ <- if size <= idx then ctx.error(src, s"index out of bounds $idx") else ().pure
            result <- if idx >= endIdx then
              EvaluatedJValue.JArray[F](src, IArray.empty).pure
            else
              val elements = for
                i <- idx until endIdx by stride
                if i < size
              yield arr.elements(i)
              EvaluatedJValue.JArray(src, IArray.unsafeFromArray(elements.toArray)).pure
          yield result
        }.flatten
    }

  case JValue.JBinaryOp(src, left, op, right) =>
    op match
    case JBinaryOperator.Op_+ =>
      type PlusOperand =
        EvaluatedJValue.JString[F] |
        EvaluatedJValue.JNum[F] |
        EvaluatedJValue.JObject[F] |
        EvaluatedJValue.JBoolean[F] |
        EvaluatedJValue.JArray[F]
      (
        eval(ctx)(left).flatMap(ctx.expect[PlusOperand](_)),
        eval(ctx)(right).flatMap(ctx.expect[PlusOperand](_)),
      ).parTupled.flatMap {
        case (op1: EvaluatedJValue.JString[F], op2) =>
          ctx.singleLinePrint(op2).map(op2 => EvaluatedJValue.JString(src, op1.string + op2))
        case (op1: EvaluatedJValue.JObject[F], op2: EvaluatedJValue.JObject[F]) =>
          op1.mixin(src, op2).widen
        case (op1: EvaluatedJValue.JArray[F], op2: EvaluatedJValue.JArray[F]) =>
          EvaluatedJValue.JArray(src, op1.elements ++ op2.elements).pure
        case (op1, op2) =>
          ctx.error(src, s"$op1, $op2, invalid operand types, expected two numbers, arrays, or objects, or one string")
      }

  case JValue.JIf(src, rawCond, trueValue, elseValue) =>
    ctx.expect[Boolean](rawCond).flatMap { cond =>
      if cond then
        eval(ctx)(trueValue)
      else
        elseValue.fold(EvaluatedJValue.JNull(src).pure)(eval(ctx))
    }

  case JValue.JError(src, rawExpr) =>
    ctx.expect[String](rawExpr).flatMap { msg =>
      ctx.error(src, msg)
    }

  case JValue.JAssert(src, rawCond, rawMsg, expr) =>
    val msg = rawMsg.fold(Option.empty[String].pure) { msg =>
      ctx.expect[String](msg).map(Some(_))
    }
    (ctx.expect[Boolean](rawCond), msg).parMapN { (cond, msgOpt) =>
      if !cond then
        ctx.error(src, msgOpt.getOrElse(s"assertion failed"))
      else
        eval(ctx)(expr)
    }.flatten

  case JValue.JArrayComprehension(src, forVar, forExpr, inExpr, condOpt) =>
    if condOpt.isDefined then
      val cond = condOpt.get
      ctx.expect[EvaluatedJValue.JArray[F]](inExpr).flatMap { array =>
        for
          res <- Sync[F].delay(Array.ofDim[Option[EvaluatedJValue[F]]](array.elements.size))
          _ <- ((0 until array.elements.size): Iterable[Int]).parTraverse { i =>
            val e = array.elements(i)
            val forCtx = ctx.bindStrict(forVar, e)
            forCtx.expect[Boolean](cond).flatMap { cond =>
              if cond then Sync[F].delay {
                res(i) = None
              } else eval(forCtx)(forExpr).flatMap { e =>
                Sync[F].delay {
                  res(i) = Some(e)
                }
              }
            }
          }
        yield
          EvaluatedJValue.JArray(src, IArray.unsafeFromArray(res.flatten))
      }
    else
      ctx.expect[EvaluatedJValue.JArray[F]](inExpr).flatMap { array =>
        for
          res <- Sync[F].delay(Array.ofDim[EvaluatedJValue[F]](array.elements.size))
          _ <- ((0 until array.elements.size): Iterable[Int]).parTraverse { i =>
            val e = array.elements(i)
            val forCtx = ctx.bindStrict(forVar, e)
            eval(forCtx)(forExpr).flatMap { e =>
              Sync[F].delay(res(i) = e)
            }
          }
        yield
          EvaluatedJValue.JArray(src, IArray.unsafeFromArray(res))
      }
  case JValue.JImport(src, file) => ctx.`import`(src, file)
  case JValue.JImportStr(src, file) => ctx.importStr(src, file).widen
  case JValue.JFunction(src, paramsDef, body) =>
    def applyArgs(defCtx: EvaluationContext[F], fnSrc: Source)(
      params: EvaluatedJValue.JFunctionParameters[F]
    ): F[EvaluatedJValue[F]] =
      val positionalArgs = params.positionalArgs
      val namedArgs = params.namedArgs
      val numGivenArgs = positionalArgs.size + namedArgs.size
      for
        _ <- if numGivenArgs > paramsDef.size then
          params.ctx.error(params.src, "to many arguments for function")
        else
          ().pure
        argMap = namedArgs.toMap
        result <- {
          var currPosArgs = positionalArgs
          var error = Option.empty[F[EvaluatedJValue[F]]]
          var i = 0
          var locals = Array.ofDim[F[(String, LazyValue[F])]](paramsDef.size)
          while error.isEmpty && i < paramsDef.size do
            val (argName, default) = paramsDef(i)
            val isGivenNamedArg = argMap.contains(argName)
            if positionalArgs.nonEmpty && isGivenNamedArg then
              error = Some(params.ctx.error(params.src, s"both positional and named arg provided for argument $argName"))
            else if currPosArgs.nonEmpty then
              locals(i) = (argName -> LazyValue.strict(currPosArgs.head)).pure
              currPosArgs = currPosArgs.tail
            else if isGivenNamedArg then
              locals(i) = (argName -> LazyValue.strict(argMap(argName))).pure
            else if default.isDefined then
              locals(i) = LazyValue(eval(defCtx)(default.get)).map(argName -> _)
            else
              error = Some(params.ctx.error(params.src, s"missing argument $argName"))
            i += 1
          error.fold(
            locals.toList.sequence.flatMap { locals =>
              eval(ctx.bind(locals))(body)
            }
          )(e => e)
        }
      yield
        result
    EvaluatedJValue.JFunction(src, paramsDef.size, applyArgs(ctx, src)).pure
  case JValue.JApply(src, loc, positionalArgs, namedArgs) =>
    ctx.expect[EvaluatedJValue.JFunction[F]](loc).flatMap { fn =>
      (
        positionalArgs.parTraverse(eval(ctx)),
        namedArgs.parTraverse((n, a) => eval(ctx)(a).map(n -> _)),
      ).parTupled.flatMap { (positionalArgs, namedArgs) =>
        fn.fn(EvaluatedJValue.JFunctionParameters(
          src,
          ctx,
          positionalArgs,
          namedArgs,
        ))
      }
    }
