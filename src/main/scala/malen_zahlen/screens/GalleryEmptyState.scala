package scaffold.screens


import tyrian.Html.*
import tyrian.*

object GalleryEmptyState {

  def apply[Msg](emptyText: String, buttonLabel: String, createMsg: Msg): Html[Msg] =
    div(`class` := s"${"container"} empty-state")(
      p(`class` := "text")(text(emptyText)),
      button(`class` := "btn is-primary", onClick(createMsg))(text(buttonLabel))
    )
}
