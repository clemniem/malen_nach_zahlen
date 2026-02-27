package scaffold

import cats.effect.IO
import scaffold.screens.{PaintByNumbersScreen, PbnMsg}
import tyrian.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object App extends TyrianIOApp[PbnMsg, PaintByNumbersScreen.PbnModel] {

  private val screen = PaintByNumbersScreen

  def router: Location => PbnMsg =
    Routing.none(PbnMsg.NoOp)

  def init(flags: Map[String, String]): (PaintByNumbersScreen.PbnModel, Cmd[IO, PbnMsg]) =
    screen.init

  def update(model: PaintByNumbersScreen.PbnModel): PbnMsg => (PaintByNumbersScreen.PbnModel, Cmd[IO, PbnMsg]) =
    screen.update(model)

  def view(model: PaintByNumbersScreen.PbnModel): Html[PbnMsg] =
    screen.view(model)

  def subscriptions(model: PaintByNumbersScreen.PbnModel): Sub[IO, PbnMsg] =
    screen.subscriptions(model)
}
