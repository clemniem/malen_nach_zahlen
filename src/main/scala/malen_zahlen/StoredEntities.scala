package scaffold

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class StoredNote(id: String, name: String, content: String)
object StoredNote {
  given Encoder[StoredNote] = deriveEncoder
  given Decoder[StoredNote] = deriveDecoder
}

object StorageKeys {
  val notes = "notes"
}
