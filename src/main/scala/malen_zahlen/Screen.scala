package scaffold

import cats.effect.IO
import tyrian.{Cmd, Html, Sub}

trait ScreenId {
  def name: String
  def title: String
}

object ScreenId {
  case object HomeId  extends ScreenId { val name = "home"; val title = "Home"   }
  case object AboutId extends ScreenId { val name = "about"; val title = "About" }
  case object NotesId extends ScreenId { val name = "notes"; val title = "Notes" }
}

sealed trait ScreenOutput

sealed trait RootMsg
object RootMsg {
  case class NavigateTo(screenId: ScreenId, output: Option[ScreenOutput]) extends RootMsg
  case class HandleScreenMsg(screenId: ScreenId, msg: Any)                extends RootMsg
}

case class NavigateNext(screenId: ScreenId, output: Option[ScreenOutput])

trait Screen {
  type Model
  type Msg

  val screenId: ScreenId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg])

  def update(model: Model): Msg => (Model, Cmd[IO, Msg])

  def view(model: Model): Html[Msg]

  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

  def wrapMsg(msg: Msg): RootMsg = RootMsg.HandleScreenMsg(screenId, msg)
}
