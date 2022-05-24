package jsonrpc4cats

import cats.effect.syntax.all.given
import cats.syntax.all.given

import fs2.{Stream, text}

import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel
import java.nio.charset.StandardCharsets

import org.typelevel.log4cats.Logger

import scala.concurrent.Future


object LowLevelMessageWriter {
  def headerWriter(out: OutputStream): PrintWriter = {
    new PrintWriter(new OutputStreamWriter(out, StandardCharsets.US_ASCII))
  }

  def write(message: LowLevelMessage): ByteBuffer = {
    val out = new ByteArrayOutputStream()
    val header = headerWriter(out)
    writeToByteBuffer(message, out, header)
  }

  def writeToByteBuffer(
      message: LowLevelMessage,
      out: ByteArrayOutputStream,
      headerOut: PrintWriter
  ): ByteBuffer = {
    message.header.foreach {
      case (key, value) =>
        headerOut.write(key)
        headerOut.write(": ")
        headerOut.write(value)
        headerOut.write("\r\n")
    }
    headerOut.write("\r\n")
    headerOut.flush()
    out.write(message.content)
    out.flush()
    val buffer = ByteBuffer.wrap(out.toByteArray, 0, out.size())
    buffer
  }

  def writeToStream[F[_]](message: LowLevelMessage): Stream[F, Byte] = {
    val strings =
      Stream
        .iterable(message.header)
        .flatMap(Stream(_, ": ", _, "\r\n"))
        .append(Stream("\r\n"))
    strings.covary[F].through(text.utf8.encode) ++ Stream.iterable(message.content)
  }


  def toByteStream[F[_]: Logger](stream: Stream[F, Message]): Stream[F, Byte] =
    stream.flatMap { msg =>
      val protocolMsg = LowLevelMessage.fromMsg(msg)
      Stream.eval(Logger[F].info(
        s"""
           |  --> header: ${protocolMsg.header.mkString(", ")}
           |  --> content: ${new String(protocolMsg.content, StandardCharsets.UTF_8)}
         """.stripMargin
       )) *> writeToStream(protocolMsg)
    }
}
