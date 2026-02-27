package scaffold.common

import cats.effect.IO
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import scaffold.screens.GalleryLayout
import tyrian.Cmd
import tyrian.cmds.LocalStorage

import scala.annotation.unused

object LocalStorageUtils {

  def save[A, M](
    key: String,
    item: A
  )(success: A => M,
    failure: (String, A) => M
  )(using Encoder[A]
  ): Cmd[IO, M] = {
    val json = item.asJson.noSpacesSortKeys
    LocalStorage.setItem(key, json) {
      case LocalStorage.Result.Success          => success(item)
      case LocalStorage.Result.Failure(message) => failure(message, item)
      case e                                    => failure(e.toString, item)
    }
  }

  def load[A, M](key: String)(success: A => M, notFound: String => M, failure: (String, String) => M)(using Decoder[A])
    : Cmd[IO, M] =
    LocalStorage.getItem(key) {
      case Left(_) =>
        notFound(key)
      case Right(result) =>
        result match {
          case found: LocalStorage.Result.Found =>
            io.circe.parser.decode[A](found.data) match {
              case Left(decodeErr) => failure(decodeErr.getMessage, key)
              case Right(decoded)  => success(decoded)
            }
        }
    }

  def saveList[A, M](
    key: String,
    items: List[A]
  )(success: List[A] => M,
    failure: (String, List[A]) => M
  )(using Encoder[List[A]]
  ): Cmd[IO, M] = {
    val json = items.asJson.noSpacesSortKeys
    LocalStorage.setItem(key, json) {
      case LocalStorage.Result.Success          => success(items)
      case LocalStorage.Result.Failure(message) => failure(message, items)
      case e                                    => failure(e.toString, items)
    }
  }

  def loadList[A, M](
    key: String
  )(success: List[A] => M,
    @unused notFound: String => M,
    failure: (String, String) => M
  )(using Decoder[List[A]]
  ): Cmd[IO, M] =
    LocalStorage.getItem(key) {
      case Left(_) =>
        success(Nil)
      case Right(result) =>
        result match {
          case found: LocalStorage.Result.Found =>
            io.circe.parser.decode[List[A]](found.data) match {
              case Left(decodeErr) => failure(decodeErr.getMessage, key)
              case Right(list)     => success(list)
            }
        }
    }

  def confirmDelete[A, M](
    listOpt: Option[List[A]],
    id: String,
    storageKey: String,
    pageSize: Int,
    currentPage: Int,
    cancelMsg: M,
    getId: A => String
  )(using Encoder[List[A]]
  ): (Option[List[A]], Int, Cmd[IO, M]) =
    listOpt match {
      case Some(list) =>
        val newList    = list.filterNot(a => getId(a) == id)
        val saveCmd    = saveList(storageKey, newList)(_ => cancelMsg, (_, _) => cancelMsg)
        val totalPages = GalleryLayout.totalPagesFor(newList.size, pageSize)
        (Some(newList), GalleryLayout.clampPage(currentPage, totalPages), saveCmd)
      case None =>
        (listOpt, currentPage, Cmd.None)
    }
}
