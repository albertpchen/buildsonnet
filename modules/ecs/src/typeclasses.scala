package ecs

sealed trait HasComponent[Record, Name <: String]:
  type Value
  inline private[ecs] def index(field: String): Int
  final type Comp = Component[Name, Value]

object Types:
  opaque type IArray[T] = Array[T]

  opaque type FileId = Int
  private val C = Component

  trait HasSourceInfo:
    val srcFile = C.file[FileId]
    val srcStart = C.start[Int]
    val srcEnd = C.end[Int]

  opaque type IdentifierId = Int
  opaque type StringId = Int
  object JString extends HasSourceInfo:
    val string = C.string[String]

  object JBoolean extends HasSourceInfo:
    val boolean = C.boolean[Boolean]

  object JNumber extends HasSourceInfo:
    val number = C.number[Double]

  object JArray extends HasSourceInfo:
    val elements = C.elements[Vector[Id]]

  object JObject extends HasSourceInfo:
    val members = C.members[IArray[Id]]

  object JObjectComprehension extends HasSourceInfo:
    val locals = C.locals[IArray[Id]]
    val key = C.key[Id]
    val forVar = C.forVar[IdentifierId]
    val inExpr = C.inExpr[Id]
    val cond = C.cond[Option[Id]]

  object JNull extends HasSourceInfo

  object JSelf extends HasSourceInfo
  object JSuper extends HasSourceInfo
  object JOuter extends HasSourceInfo

  object JIdentifier extends HasSourceInfo:
    val identifier = C.identifier[IdentifierId]

  object JGetField extends HasSourceInfo:
    val field = C.field[IdentifierId]

  object JIndex extends HasSourceInfo:
    val index = C.index[Id]

  object JSlice extends HasSourceInfo:
    val startIndex = C.startIndex[Id]
    val endIndex = C.endIndex[Option[Id]]
    val stride = C.stride[Option[Id]]

  object JApply extends HasSourceInfo:
    val positionalArgs = C.positionalArgs[IArray[Id]]
    val namedArgs = C.namedArgs[IArray[(String, Id)]]

  opaque type BinaryOp = Int
  object JBinaryOp extends HasSourceInfo:
    val left = C.left[Id]
    val right = C.right[Id]
    val op = C.op[BinaryOp]

  opaque type UnaryOp = Int
  object JUnaryOp extends HasSourceInfo:
    val expr = C.expr[Id]
    val op = C.op[UnaryOp]

  object JLocal extends HasSourceInfo:
    val name = C.name[IdentifierId]
    val value = C.value[Id]
    val result = C.result[Id]

  type ParamList = IArray[(String, Option[Id])]
  object JFunction extends HasSourceInfo:
    val params = C.params[ParamList]
    val body = C.body[Id]

  object JIf extends HasSourceInfo:
    val cond = C.cond[Id]
    val trueValue = C.trueValue[Id]
    val elseValue = C.elseValue[Id]

  object JError extends HasSourceInfo:
    val expr = C.expr[Id]

  object JAssert extends HasSourceInfo:
    val cond = C.cond[Id]
    val msg = C.msg[Option[Id]]
    val expr = C.expr[Id]

  object JImport extends HasSourceInfo:
    val file = C.file[StringId]

  object JImportString extends HasSourceInfo:
    val file = C.file[StringId]

  object JArrayComprehension extends HasSourceInfo:
    val forVar = C.forVar[StringId]
    val forExpr = C.forExpr[Id]
    val inExpr = C.inExpr[Id]
    val cond = C.cond[Option[Id]]

  trait NodeSpec:
    final class Component[Name, Value] private()

  trait Context[T]:
    val node: T
    val arr: Array[Int]

  extension[T <: NodeSpec](id: Id)(using ctx: Context[T])
    inline def get[Name, Value](c: ctx.node.Component[Name, Value]): Value =
      ???

/*

system.add[JString.type](
  srcFile = "asdf.jsonnet",
  srcStart = 123,
  srcEnd = 456,
  string = "foobar",
)

object JArrayEvalPass:
  type NodeType = JArray.type

  inline def eval(ctx: EvalPassCtx, str: String): String =
    elements.map(ctx.recurse)

evalPass.register[JArray.type] { (ctx, elements: IArray[Id]) =>
  elements.map(ctx.recurse)
}

// impliclty convert to (_: Context[JObject.type]) ?=> (id: Id) =>
evalPass.register[JObject.type] { (id: Id) =>
  val members = id.get(JObject.members).flatMap { i =>
    ctx.getOpt[JField.type](i) { (src, rawKey, plus, isHidden, value) =>
    }
  }
}
*/
