package jsonrpc4cats

import cats.effect.Sync
import cats.effect.syntax.all.given
import cats.syntax.all.given

import fs2.{Chunk, Pull, Stream, text}

import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel
import java.nio.charset.StandardCharsets

import org.typelevel.log4cats.Logger

import scala.collection.mutable
import scala.concurrent.Future


object LowLevelMessageWriter {
  def write(message: LowLevelMessage): ByteBuffer =
    val out = new ByteArrayOutputStream()
    val header = headerWriter(out)
    writeToByteBuffer(message, out, header)

  private def headerWriter(out: OutputStream): PrintWriter =
    new PrintWriter(new OutputStreamWriter(out, StandardCharsets.US_ASCII))

  private def writeToByteBuffer(
    message: LowLevelMessage,
    out: ByteArrayOutputStream,
    headerOut: PrintWriter
  ): ByteBuffer =
    message.header.foreach { (key, value) =>
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


  private def pull[F[_]: Sync: Logger](stream: Stream[F, Message]): Pull[F, Byte, Unit] =
    stream.pull.uncons1.flatMap {
      case None => Pull.done
      case Some((head, tail)) =>
        val protocolMsg = LowLevelMessage.fromMsg(head)
        Pull.eval(Logger[F].info(
          s"""
             |  --> header: ${protocolMsg.header.mkString(", ")}
             |  --> content: ${new String(protocolMsg.content, StandardCharsets.UTF_8)}
           """.stripMargin
        )) >> Pull.output(Chunk.byteBuffer(write(protocolMsg))) >>
        pull(tail)
    }

  def toByteStream[F[_]: Sync: Logger](stream: Stream[F, Message]): Stream[F, Byte] =
    pull(stream).stream
}
