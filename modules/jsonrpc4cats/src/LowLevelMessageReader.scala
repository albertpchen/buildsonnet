package jsonrpc4cats

import cats.effect.Sync
import cats.syntax.all.given

import fs2.{Chunk, Pull, Stream}
import fs2.io.readInputStream

import java.io.InputStream
import java.nio.charset.StandardCharsets

import org.typelevel.log4cats.Logger

import scala.collection.mutable.ArrayBuffer

private enum ReadHeadersResult:
  case NeedsMoreData
  case ContinueProcessing

private enum ReadContentResult:
  case NeedsMoreData
  case ContinueProcessing(msg: LowLevelMessage)

final class LowLevelMessageReader[F[_]: Sync: Logger] {
  private val buffer = ArrayBuffer.empty[Byte]

  private var contentLength = -1
  private var header = Map.empty[String, String]

  private val EmptyPair = "" -> ""


  def acceptChunk(chunk: Chunk[Byte]): F[Unit] =
    Sync[F].delay { buffer ++= chunk.iterator }

  val processMessage: F[Option[LowLevelMessage]] =
    for
      contentLength <- Sync[F].delay(contentLength)
      result <-
        if contentLength < 0 then
          for
            readHeadersResult <- readHeaders
            result <- readHeadersResult match
              case ReadHeadersResult.NeedsMoreData => None.pure
              case ReadHeadersResult.ContinueProcessing => processMessage
          yield
            result
        else
          for
            contentResult <- readContent.map {
              case ReadContentResult.NeedsMoreData => None
              case ReadContentResult.ContinueProcessing(msg) => Some(msg)
            }
            _ <- Sync[F].delay { if contentResult.isDefined then buffer.trimToSize() }
          yield
            contentResult
    yield result

  private def atDelimiter(idx: Int): Boolean = {
    buffer.size >= idx + 4 &&
    buffer(idx) == '\r' &&
    buffer(idx + 1) == '\n' &&
    buffer(idx + 2) == '\r' &&
    buffer(idx + 3) == '\n'
  }

  private val readHeaders: F[ReadHeadersResult] = Sync[F].delay {
    if buffer.size < 4 then
      None
    else
      var i = 0
      while i + 4 < buffer.size && !atDelimiter(i) do
        i += 1
      if !atDelimiter(i) then
        None
      else
        val bytes = new Array[Byte](i)
        buffer.copyToArray(bytes)
        buffer.remove(0, i + 4)
        Some(new String(bytes, StandardCharsets.US_ASCII))
  }.flatMap {
    case None => None.pure
    case Some(headers) =>
      // Parse other headers in JSON-RPC messages even if we only use `Content-Length` below
      headers
        .split("\r\n")
        .filterNot(_.trim.isEmpty)
        .map { line =>
          line.split(":") match {
            case Array(key, value) => (key.trim -> value.trim).pure
            case _ =>
              Logger[F].error(s"Malformed input: $line").as(EmptyPair)
          }
        }
        .toList
        .sequence
        .map(pairs => Some(pairs.toMap))
  }.flatMap {
    case None => ReadHeadersResult.NeedsMoreData.pure

    case Some(pairs) if !pairs.contains("Content-Length") =>
      Logger[F].error(s"Missing Content-Length key in headers $pairs").as(ReadHeadersResult.NeedsMoreData)

    case Some(pairs) =>
      val contentLenStr = pairs("Content-Length")
      for
        nOpt <- Sync[F].delay(Some(contentLenStr.toInt)).handleErrorWith {
          case _: NumberFormatException =>
            Logger[F].error(s"Expected Content-Length to be a number, obtained $contentLenStr").as(None)
        }
        result <- nOpt match
          case None => ReadHeadersResult.NeedsMoreData.pure
          case Some(n) => Sync[F].delay {
            contentLength = n
            header = pairs
            ReadHeadersResult.ContinueProcessing
          }
      yield result
  }

  private val readContent: F[ReadContentResult] = Sync[F].delay {
    if contentLength > buffer.size then
      ReadContentResult.NeedsMoreData
    else
      val contentBytes = new Array[Byte](contentLength)
      buffer.copyToArray(contentBytes)
      buffer.remove(0, contentLength)
      contentLength = -1
      ReadContentResult.ContinueProcessing(LowLevelMessage(header, contentBytes))
  }
}


object LowLevelMessageReader:
  private case class State[F[_]: Logger](
    data: ArrayBuffer[Byte],
    reader: LowLevelMessageReader[F],
  )
  private def pull[F[_]: Sync: Logger](
    stream: Stream[F, Byte],
  ): Pull[F, LowLevelMessage, Unit] =
    Pull.eval(Sync[F].delay(LowLevelMessageReader())).flatMap { reader =>
      pull(stream, reader)
    }

  private def pull[F[_]: Sync: Logger](
    stream: Stream[F, Byte],
    reader: LowLevelMessageReader[F],
  ): Pull[F, LowLevelMessage, Unit] =
    stream.pull.uncons.flatMap {
      case None => Pull.done
      case Some((head, tail)) =>
        def processUntilEmpty: Pull[F, LowLevelMessage, Unit] =
          Pull.eval(reader.processMessage).map {
            case None => pull(tail, reader)
            case Some(msg) => Pull.output1(msg) >> processUntilEmpty
          }
        Pull.eval(reader.acceptChunk(head)) >> processUntilEmpty
    }

  def fromChannel[F[_]: Sync: Logger](channel: F[InputStream]): Stream[F, LowLevelMessage] =
    val byteStream = readInputStream(channel, chunkSize = 8192)
    pull(byteStream).stream

  def toMessageStream[F[_]: Sync: Logger](bytes: Stream[F, Byte]): Stream[F, LowLevelMessage] =
    pull(bytes).stream
