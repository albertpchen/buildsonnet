package jsonrpc4cats
package testprotocol

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter, JsonValueCodec}

import org.scalacheck.Gen

import scala.deriving.Mirror
import scala.compiletime.summonInline

final case class TestParam[A](id: Int, value: A)

object TestParam:
  def makeCodec[A](codecA: JsonValueCodec[A]): JsonValueCodec[TestParam[A]] = {
    def fieldName(i: Int): String = i match
    case 0 => "id"
    case 1 => "value"

    def d0(in: JsonReader, defaultValue: TestParam[A]): TestParam[A] =
      if !in.isNextToken('{') then in.readNullOrTokenError[TestParam[A]](defaultValue, '{')
      else
        var id: Int = 0
        var value: A | Null = null
        var mask: Int = 3

        if !in.isNextToken('}') then
          in.rollbackToken()
          var l: Int = -1
          while l < 0 || in.isNextToken(',') do
            l = in.readKeyAsCharBuf()
            if in.isCharBufEqualsTo(l, "id") then
              if (1 & mask) != 0 then mask = mask ^ 1 else in.duplicatedKeyError(l)
              id = in.readInt()
            else if in.isCharBufEqualsTo(l, "value") then
              if (2 & mask) != 0 then mask = mask ^ 2 else in.duplicatedKeyError(l)
              value = codecA.decodeValue(in, value.asInstanceOf[A])
            else in.skip()

          if !in.isCurrentToken('}') then in.objectEndOrCommaError()

        if ((mask & 3) != 0) then
          in.requiredFieldError(fieldName(Integer.numberOfTrailingZeros(mask & 3)))

        TestParam[A](id, value.asInstanceOf[A])

    def e0(x: TestParam[A], out: JsonWriter): Unit =
      out.writeObjectStart()
      out.writeNonEscapedAsciiKey("id")
      out.writeVal(x.id)
      out.writeNonEscapedAsciiKey("value")
      codecA.encodeValue(x.value, out)
      out.writeObjectEnd()

    new JsonValueCodec[TestParam[A]] {
      def nullValue: TestParam[A] = null
      def decodeValue(in: JsonReader, default: TestParam[A]): TestParam[A] = d0(in, default)
      def encodeValue(x: TestParam[A], out: JsonWriter): Unit = e0(x, out)
    }
  }

case class OpenConnectionParams(
  num: Int,
)

case class OpenConnectionReturn(
  num: Int,
)

case class CompileParams(
  name: String,
  number: Int,
)

case class CompileReturn(
  result: Boolean,
  args: List[String],
)

case class WarnParams(
  message: String,
)

sealed trait TestAction:
  type A
  type B
  def endpoint: Endpoint[TestParam[A], B]

sealed trait ExpectNotification extends TestAction:
  final type B = Unit
  def notification: A
  final def notificationWithId(id: Int): TestParam[A] = TestParam(id, notification)

object ExpectNotification:
  type Aux[_A] = ExpectNotification {
    type A = _A
  }
  def apply[_A](
    _endpoint: Endpoint[_A, Unit],
    expectedNotification: _A,
  ): Aux[_A] = new ExpectNotification {
    type A = _A
    given JsonValueCodec[A] = _endpoint.codecA
    val endpoint = Endpoint[TestParam[A], B](_endpoint.method)(
      using
      TestParam.makeCodec(_endpoint.codecA),
      _endpoint.codecB,
    )
    val notification = expectedNotification
  }

sealed trait ExpectRequest extends TestAction:
  def request: A
  def response: B
  final def requestWithId(id: Int): TestParam[A] = TestParam(id, request)

object ExpectRequest:
  def apply[_A, _B](
    _endpoint: Endpoint[_A, _B],
    expectedRequest: _A,
    expectedResponse: _B,
  ): ExpectRequest = new ExpectRequest {
    type A = _A
    type B = _B
    given JsonValueCodec[A] = _endpoint.codecA
    val endpoint = Endpoint[TestParam[A], B](_endpoint.method)(
      using
      TestParam.makeCodec(_endpoint.codecA),
      _endpoint.codecB,
    )
    val request = expectedRequest
    val response = expectedResponse
  }

inline def genExpectNotification[A](endpoint: Endpoint[A, Unit]): Gen[ExpectNotification] =
  GenDerivers
    .deriveGen[A](using summonInline[Mirror.Of[A]])
    .map(ExpectNotification(endpoint, _))

inline def genExpectRequest[A, B](endpoint: Endpoint[A, B]): Gen[ExpectRequest] =
  for
    request <- GenDerivers.deriveGen[A](using summonInline[Mirror.Of[A]])
    response <- GenDerivers.deriveGen[B](using summonInline[Mirror.Of[B]])
  yield ExpectRequest(endpoint, request, response)
