package root

type JType =
    Boolean
  | Double
  | String
  | Unit
  | Seq[EvaluatedJValue]
  | EvaluatedJValue.JObject
  | EvaluatedJValue.JFunction

type JTypeOf[T] = T match
  case EvaluatedJValue.JBoolean => Boolean
  case EvaluatedJValue.JNull.type => Unit
  case EvaluatedJValue.JString => String
  case EvaluatedJValue.JNum => Double
  case EvaluatedJValue.JArray => Seq[EvaluatedJValue]
  case EvaluatedJValue.JObject => EvaluatedJValue.JObject
  case EvaluatedJValue.JFunction => EvaluatedJValue.JFunction

type EvaluatedJTypeOf[T] = T match
  case Boolean                   => EvaluatedJValue.JBoolean
  case Double                    => EvaluatedJValue.JNull.type
  case String                    => EvaluatedJValue.JString
  case Unit                      => EvaluatedJValue.JNum
  case Seq[EvaluatedJValue]      => EvaluatedJValue.JArray
  case EvaluatedJValue.JObject   => EvaluatedJValue.JObject
  case EvaluatedJValue.JFunction => EvaluatedJValue.JFunction
