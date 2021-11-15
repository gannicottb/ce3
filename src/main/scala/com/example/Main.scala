package com.example

import cats.syntax.all._
import cats.effect.IOApp
import cats.effect.IO

object Main extends IOApp.Simple {

  def run: IO[Unit] = for {
    ctr <- IO.ref[Int](0)
    nFibers = 10

    _ <- IO.parSequenceN(nFibers)(
      List
        .fill(nFibers)(())
        .map(_ =>
          ctr.updateAndGet(_ + 1).flatMap(n => IO.println(s"Update ctr to $n"))
        )
    )

    result <- ctr.get
    _ <- IO.println(s"Result is: $result")

    total <- FileIO.copy[IO]("file1.txt", "file2.txt")
    _ <- IO.println(s"$total bytes copied.")
  } yield ()

}
