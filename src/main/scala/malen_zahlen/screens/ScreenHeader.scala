package scaffold.screens


import tyrian.Html.*
import tyrian.*

object ScreenHeader {

  def apply[Msg](
    title: String,
    buttonRow: Html[Msg],
    nameRow: Option[Html[Msg]],
    shortHeader: Boolean
  ): Html[Msg] = {
    val headerClass = if (shortHeader) "screen-header screen-header--short" else "screen-header"
    val children    = Seq(h2(`class` := "screen-title")(text(title)), buttonRow) ++ nameRow.toSeq
    div(`class` := headerClass)(children*)
  }

  def nameRowInput[Msg](
    nameValue: String,
    setMsg: String => Msg,
    inputId: Option[String],
    extraRowClass: String
  ): Html[Msg] = {
    val rowClass = s"${"field"} screen-header-name-row $extraRowClass".trim
    val attrs = Seq(
      `type`      := "text",
      placeholder := "Name",
      value       := nameValue,
      onInput(setMsg),
      `class` := s"${"input"} screen-header-name-input"
    ) ++ inputId.map(i => id := i)
    div(`class` := rowClass)(input(attrs*))
  }
}
