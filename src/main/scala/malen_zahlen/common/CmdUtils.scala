package scaffold.common

import cats.effect.IO
import tyrian.Cmd

object CmdUtils {

  def run[A, M](io: IO[A], toMsg: A => M, onError: Throwable => M): Cmd[IO, M] =
    Cmd.Run(
      io.attempt,
      {
        case Right(v) => toMsg(v)
        case Left(e)  => onError(e)
      })

  def fireAndForget[M](io: IO[Unit], noOp: M, onError: Throwable => M): Cmd[IO, M] =
    run[Unit, M](io, _ => noOp, onError)
}
