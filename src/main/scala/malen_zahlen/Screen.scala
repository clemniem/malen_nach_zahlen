package scaffold

import cats.effect.IO
import tyrian.{Cmd, Html, Sub}

trait Screen {
  type Model
  type Msg

  def init: (Model, Cmd[IO, Msg])

  def update(model: Model): Msg => (Model, Cmd[IO, Msg])

  def view(model: Model): Html[Msg]

  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None
}
