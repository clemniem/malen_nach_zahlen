package scaffold.screens


import tyrian.Html.*
import tyrian.*

object GalleryLayout {

  def backButtonLabel[Msg](arrow: String, label: String): Html[Msg] =
    span(span(`class` := "btn-arrow")(text(arrow)), text(" " + label))

  def nextButtonLabel[Msg](label: String, arrow: String): Html[Msg] =
    span(text(label + " "), span(`class` := "btn-arrow")(text(arrow)))

  def backButton[Msg](msg: Msg, label: String): Html[Msg] =
    button(`class` := "btn", onClick(msg))(backButtonLabel("←", label))

  def totalPagesFor(size: Int, pageSize: Int): Int =
    if (size <= 0) 1 else ((size - 1) / pageSize) + 1

  def clampPage(current: Int, totalPages: Int): Int =
    current.min(totalPages).max(1)

  def sliceForPage[A](list: List[A], currentPage: Int, pageSize: Int): (List[A], Int, Int) = {
    val total = totalPagesFor(list.size, pageSize)
    val page  = clampPage(currentPage, total)
    val start = (page - 1) * pageSize
    val slice = list.slice(start, start + pageSize)
    (slice, page, total)
  }

  def galleryActionsRow[Msg](buttons: Html[Msg]*): Html[Msg] =
    div(`class` := "gallery-actions")(buttons*)

  def galleryDeleteConfirm[Msg](confirmMessage: String, onConfirm: Msg, onCancel: Msg): Html[Msg] =
    div(`class` := "gallery-delete-confirm")(
      span(`class` := "delete-confirm-text nes-text")(text(confirmMessage)),
      button(`class` := "btn is-error", onClick(onConfirm))(text("Yes")),
      button(`class` := "btn", onClick(onCancel))(text("Cancel"))
    )

  val galleryContentClass = "gallery-content"
  val galleryListClass    = "gallery-list"
  val defaultPageSize: Int = 5

  def listWithAddActionAndPagination[Msg](
    addAction: Html[Msg],
    entriesForCurrentPage: Iterable[Html[Msg]],
    currentPage: Int,
    totalPages: Int,
    onPreviousPage: Msg,
    onNextPage: Msg
  ): Html[Msg] = {
    val paginationBar =
      if (totalPages <= 1) None
      else
        Some(
          div(`class` := "gallery-pagination")(
            button(
              `class` := (if (currentPage <= 1) s"${"btn"} btn-disabled" else "btn"),
              onClick(onPreviousPage)
            )(backButtonLabel("←", "Previous")),
            span(`class` := "gallery-pagination-label nes-text")(
              text(s"Page $currentPage of $totalPages")
            ),
            button(
              `class` := (if (currentPage >= totalPages) s"${"btn"} btn-disabled" else "btn"),
              onClick(onNextPage)
            )(nextButtonLabel("Next", "→"))
          )
        )
    val children: Seq[Html[Msg]] =
      addAction +: div(`class` := "gallery-list-entries")(entriesForCurrentPage.toSeq*) +: paginationBar.toList
    div(`class` := galleryListClass)(children*)
  }

  def apply[Msg](
    title: String,
    backButton: Html[Msg],
    content: Html[Msg],
    shortHeader: Boolean,
    nextButton: Option[Html[Msg]]
  ): Html[Msg] = {
    val headerClass = if (shortHeader) "screen-header screen-header--short" else "screen-header"
    val headerButtons = nextButton match {
      case Some(next) => div(`class` := "flex-row", style := "gap: 0.5rem;")(backButton, next)
      case None       => backButton
    }
    div(`class` := "screen-container")(
      div(`class` := headerClass)(
        h1(`class` := "screen-title")(text(title)),
        headerButtons
      ),
      div(`class` := s"$galleryContentClass screen-container-inner")(content)
    )
  }
}
