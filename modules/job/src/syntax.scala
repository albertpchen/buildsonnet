package buildsonnet.job

import cats.effect.Sync

import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.FileTime

import java.security.{MessageDigest, DigestInputStream}

object syntax:
  extension [F[_]: Sync](path: Path)
    def exists: Boolean = Files.exists(path)

    def fileTime: FileTime =
      Files
        .readAttributes(path, "lastModifiedTime")
        .get("lastModifiedTime")
        .asInstanceOf[FileTime]

    // TODO: return option instead? in case of non-existent path
    def md5Hash: F[String] =
      Sync[F].delay(md5HashUnsafe)

    def md5HashUnsafe: String =
      val buffer = Array.ofDim[Byte](8192)
      val md5 = MessageDigest.getInstance("MD5")

      val dis = new DigestInputStream(Files.newInputStream(path), md5)
      try
        while dis.read(buffer) != -1 do ()
        dis
      finally
        dis.close()

      md5.digest.map("%02x".format(_)).mkString

  extension (str: String)
    def md5Hash(charset: String): Array[Byte] =
      MessageDigest
        .getInstance("MD5")
        .digest(str.getBytes(charset))
