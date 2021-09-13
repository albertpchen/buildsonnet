package root

enum JType[T <: EvaluatedJValue]:
  case JBoolean extends JType[EvaluatedJValue.JBoolean]
  case JNum extends JType[EvaluatedJValue.JNum]
  case JString extends JType[EvaluatedJValue.JString]
  case JNull extends JType[EvaluatedJValue.JNull]
  case JArray extends JType[EvaluatedJValue.JArray]
  case JObject extends JType[EvaluatedJValue.JObject]
  case JFunction extends JType[EvaluatedJValue.JFunction]
