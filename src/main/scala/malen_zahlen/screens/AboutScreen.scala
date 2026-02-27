package scaffold.screens

import cats.effect.IO
import scaffold.{NavigateNext, Screen, ScreenId, ScreenOutput}
import scaffold.common.CmdUtils

import org.scalajs.dom
import tyrian.Html.*
import tyrian.*
import scala.scalajs.js

object AboutScreen extends Screen {
  type Model = Boolean
  type Msg   = AboutMsg | NavigateNext

  val screenId: ScreenId = ScreenId.AboutId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (false, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case AboutMsg.ShowRefreshConfirm =>
      (true, Cmd.None)
    case AboutMsg.ConfirmRefresh =>
      (model, CmdUtils.fireAndForget(refreshAppFromSW, AboutMsg.NoOp, _ => AboutMsg.NoOp))
    case AboutMsg.CancelRefresh =>
      (false, Cmd.None)
    case AboutMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.HomeId, None)))
    case AboutMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def refreshAppFromSW: IO[Unit] = IO.delay {
    val f = js.Dynamic.global.selectDynamic("refreshApp")
    if (js.typeOf(f) == "function") {
      f.asInstanceOf[js.Function0[Unit]]()
    } else {
      dom.window.location.reload()
    }
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text("About")),
        GalleryLayout.backButton(AboutMsg.Back, "Home")
      ),
      div(`class` := "about-content")(
        p(`class` := "text")(text("This project was created with the Tyrian Scaffold.")),
        p(`class` := "text")(text("An Elm-style Scala.js SPA with screen navigation and LocalStorage persistence.")),
        h2(`class` := "about-heading")(text("Libraries & tools")),
        div(`class` := "about-tools")(toolsTable),
        h2(`class` := "about-heading")(text("Get latest version")),
        p(`class` := "text")(text("After a deploy, the browser may serve cached files.")),
        p(`class` := "text")(text("Use the button below to unregister the cache and reload.")),
        (
          if (model)
            div(`class` := "about-refresh-confirm")(
              p(`class` := "about-refresh-confirm-text")(
                text("Unregister cache and reload now? The page will refresh.")
              ),
              div(`class` := "flex-row flex-row--tight")(
                button(`class` := "btn is-primary", onClick(AboutMsg.ConfirmRefresh))(text("Yes, refresh")),
                button(`class` := "btn", onClick(AboutMsg.CancelRefresh))(text("Cancel"))
              )
            )
          else
            button(`class` := "btn", onClick(AboutMsg.ShowRefreshConfirm))(text("Refresh app"))
        )
      )
    )

  private def toolsTable: Html[Msg] = {
    val rows = List(
      ("Tyrian", "Elm-style UI framework for Scala.js", "https://github.com/PurpleKingdomGames/tyrian"),
      ("NES.css", "Retro pixel-art CSS framework", "https://nostalgic-css.github.io/NES.css/"),
      ("Circe", "JSON encoding / decoding", "https://circe.github.io/circe/"),
      ("Scala.js", "Scala compiled to JavaScript", "https://www.scala-js.org/"),
      ("Parcel", "Dev server and bundler", "https://parceljs.org/")
    )
    div(`class` := "about-table-wrap")(
      table(`class` := "about-table")(
        thead(
          tr(
            th(`class` := "text")(text("Library")),
            th(`class` := "text")(text("Purpose"))
          )
        ),
        tbody(
          rows.map { case (name, purpose, url) =>
            tr(
              td(`class` := "text")(
                a(
                  href := url,
                  Attribute("target", "_blank"),
                  Attribute("rel", "noopener noreferrer"),
                  `class` := "about-link"
                )(text(name))
              ),
              td(`class` := "text")(text(purpose))
            )
          }*
        )
      )
    )
  }
}

enum AboutMsg {
  case ShowRefreshConfirm
  case ConfirmRefresh
  case CancelRefresh
  case Back
  case NoOp
}
