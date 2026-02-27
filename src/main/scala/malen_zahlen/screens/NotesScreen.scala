package scaffold.screens

import cats.effect.IO
import scaffold.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredNote}
import scaffold.common.LocalStorageUtils

import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

object NotesScreen extends Screen {
  type Model = NotesModel
  type Msg   = NotesMsg | NavigateNext

  val screenId: ScreenId = ScreenId.NotesId

  private val pageSize = GalleryLayout.defaultPageSize

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (
      NotesModel(None, 1, None, "", ""),
      LocalStorageUtils.loadList[StoredNote, Msg](StorageKeys.notes)(
        NotesMsg.Loaded.apply,
        _ => NotesMsg.Loaded(Nil),
        (msg, _) => NotesMsg.Error(msg)
      )
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case NotesMsg.Loaded(notes) =>
      (model.copy(notes = Some(notes)), Cmd.None)

    case NotesMsg.SetNewName(v) =>
      (model.copy(newName = v), Cmd.None)

    case NotesMsg.SetNewContent(v) =>
      (model.copy(newContent = v), Cmd.None)

    case NotesMsg.Create =>
      val trimmed = model.newName.trim
      if (trimmed.isEmpty) (model, Cmd.None)
      else {
        val note    = StoredNote(js.Dynamic.global.crypto.randomUUID().asInstanceOf[String], trimmed, model.newContent)
        val updated = model.notes.getOrElse(Nil) :+ note
        (
          model.copy(notes = Some(updated), newName = "", newContent = ""),
          LocalStorageUtils.saveList(StorageKeys.notes, updated)(_ => NotesMsg.Saved, (msg, _) => NotesMsg.Error(msg))
        )
      }

    case NotesMsg.AskDelete(id) =>
      (model.copy(pendingDeleteId = Some(id)), Cmd.None)

    case NotesMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)

    case NotesMsg.ConfirmDelete(id) =>
      val (newList, newPage, cmd) =
        LocalStorageUtils.confirmDelete[StoredNote, Msg](
          model.notes,
          id,
          StorageKeys.notes,
          pageSize,
          model.currentPage,
          NotesMsg.CancelDelete,
          _.id
        )
      (model.copy(notes = newList, currentPage = newPage, pendingDeleteId = None), cmd)

    case NotesMsg.PreviousPage =>
      (model.copy(currentPage = (model.currentPage - 1).max(1)), Cmd.None)

    case NotesMsg.NextPage =>
      val total = GalleryLayout.totalPagesFor(model.notes.map(_.size).getOrElse(0), pageSize)
      (model.copy(currentPage = (model.currentPage + 1).min(total)), Cmd.None)

    case NotesMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.HomeId, None)))

    case NotesMsg.Saved =>
      (model, Cmd.None)

    case NotesMsg.Error(_) =>
      (model, Cmd.None)

    case NotesMsg.NoOp =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    GalleryLayout(
      "Notes",
      GalleryLayout.backButton(NotesMsg.Back, "Home"),
      viewContent(model),
      false,
      None
    )

  private def viewContent(model: Model): Html[Msg] =
    model.notes match {
      case None =>
        p(`class` := "text")(text("Loading..."))
      case Some(notes) if notes.isEmpty =>
        div(
          createForm(model),
          GalleryEmptyState("No notes yet.", "+ Add your first note", NotesMsg.NoOp)
        )
      case Some(notes) =>
        val (slice, page, totalPages) = GalleryLayout.sliceForPage(notes, model.currentPage, pageSize)
        GalleryLayout.listWithAddActionAndPagination(
          createForm(model),
          slice.map(n => noteCard(n, model.pendingDeleteId)),
          page,
          totalPages,
          NotesMsg.PreviousPage,
          NotesMsg.NextPage
        )
    }

  private def createForm(model: Model): Html[Msg] =
    div(`class` := s"${"container"} gallery-card", style := "flex-direction: column; align-items: stretch;")(
      div(`class` := "field-block")(
        label(`class` := "label-block")(text("Name")),
        input(
          `class` := "input",
          `type`  := "text",
          placeholder := "Note name",
          value := model.newName,
          onInput(NotesMsg.SetNewName.apply)
        )
      ),
      div(`class` := "field-block")(
        label(`class` := "label-block")(text("Content")),
        input(
          `class` := "input",
          `type`  := "text",
          placeholder := "Write something...",
          value := model.newContent,
          onInput(NotesMsg.SetNewContent.apply)
        )
      ),
      button(`class` := "btn is-primary", onClick(NotesMsg.Create))(text("+ Add Note"))
    )

  private def noteCard(note: StoredNote, pendingDeleteId: Option[String]): Html[Msg] =
    div(`class` := s"${"container"} gallery-card", style := "flex-direction: column; align-items: stretch;")(
      div(`class` := "gallery-card-body")(
        span(`class` := "gallery-card-title")(text(note.name)),
        span(`class` := "gallery-card-meta")(text(note.content))
      ),
      (
        if (pendingDeleteId.contains(note.id))
          GalleryLayout.galleryDeleteConfirm(
            s"Delete ${note.name}?",
            NotesMsg.ConfirmDelete(note.id),
            NotesMsg.CancelDelete
          )
        else
          GalleryLayout.galleryActionsRow(
            button(`class` := "btn is-error", onClick(NotesMsg.AskDelete(note.id)))(text("Delete"))
          )
      )
    )
}

final case class NotesModel(
  notes: Option[List[StoredNote]],
  currentPage: Int,
  pendingDeleteId: Option[String],
  newName: String,
  newContent: String)

enum NotesMsg {
  case Loaded(notes: List[StoredNote])
  case SetNewName(value: String)
  case SetNewContent(value: String)
  case Create
  case AskDelete(id: String)
  case CancelDelete
  case ConfirmDelete(id: String)
  case PreviousPage
  case NextPage
  case Back
  case Saved
  case Error(message: String)
  case NoOp
}
