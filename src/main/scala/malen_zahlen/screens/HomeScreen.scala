package scaffold.screens

import cats.effect.IO
import scaffold.{NavigateNext, Screen, ScreenId, ScreenOutput}

import tyrian.Html.*
import tyrian.*

object HomeScreen extends Screen {
  type Model = Unit
  type Msg   = HomeMsg | NavigateNext

  val screenId: ScreenId = ScreenId.HomeId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    ((), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case HomeMsg.GoTo(target) =>
      (model, Cmd.Emit(NavigateNext(target, None)))
    case HomeMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      h1(`class` := "screen-title")(text("Tyrian Scaffold")),
      p(`class` := s"${"text"} screen-intro")(
        text("Hello, World! This is your Scala.js + Tyrian starter project.")
      ),
      div(`class` := "flex-col flex-col--gap-1 screen-container-inner")(
        linkCard("Notes", ScreenId.NotesId, "A CRUD demo with LocalStorage persistence"),
        linkCard("About", ScreenId.AboutId, "Learn about the tech stack and tools")
      )
    )

  private def linkCard(title: String, target: ScreenId, desc: String): Html[Msg] =
    button(
      `class` := s"${"btn"} link-card",
      onClick(HomeMsg.GoTo(target))
    )(
      span(`class` := "link-card-title")(text(title)),
      span(`class` := "link-card-desc")(text(desc))
    )
}

enum HomeMsg {
  case GoTo(screenId: ScreenId)
  case NoOp
}
