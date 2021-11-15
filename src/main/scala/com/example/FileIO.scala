package com.example

import cats.effect.IO
import cats.effect.Sync
import cats.syntax.all._
import cats.effect.Resource
import java.io._

object FileIO {
  def inputStream[F[_]: Sync](f: File): Resource[F, FileInputStream] =
    Resource.make {
      Sync[F].blocking(new FileInputStream(f)) // build
    } { inStream =>
      Sync[F]
        .blocking(inStream.close())
        .handleErrorWith(_ => Sync[F].unit) // release
    }

  def outputStream[F[_]: Sync](f: File): Resource[F, FileOutputStream] =
    Resource.make {
      Sync[F].blocking(new FileOutputStream(f)) // build
    } { outStream =>
      Sync[F]
        .blocking(outStream.close())
        .handleErrorWith(_ => Sync[F].unit) // release
    }

  def inputOutputStreams[F[_]: Sync](
      in: File,
      out: File
  ): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transmit[F[_]: Sync](
      origin: InputStream,
      destination: OutputStream,
      buffer: Array[Byte],
      acc: Long
  ): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.size))
      count <-
        if (amount > -1)
          Sync[F].blocking(destination.write(buffer, 0, amount)) >> transmit(
            origin,
            destination,
            buffer,
            acc + amount
          )
        else
          Sync[F].pure(
            acc
          ) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted // Returns the actual amount of bytes transmitted

  def transfer[F[_]: Sync](
      origin: InputStream,
      destination: OutputStream
  ): F[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  def copy[F[_]: Sync](originName: String, destinationName: String): F[Long] = {
    val origin = new File(originName)
    val destination = new File(destinationName)
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }
  }

}
