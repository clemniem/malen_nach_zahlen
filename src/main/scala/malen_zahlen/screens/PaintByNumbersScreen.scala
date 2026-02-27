package scaffold.screens

import cats.effect.IO
import scaffold.Screen
import scaffold.common.image.*
import scaffold.common.pdf.{Instruction, JsPDF}
import org.scalajs.dom
import tyrian.Html.*
import tyrian.*

sealed trait PbnMsg
object PbnMsg {
  case object FileInputChanged                            extends PbnMsg
  case class FileSelected(dataUrl: String)                extends PbnMsg
  case class SetColorCount(n: Int)                        extends PbnMsg
  case object Generate                                    extends PbnMsg
  case class ProcessingDone(result: PbnResult)            extends PbnMsg
  case class ProcessingFailed(error: String)              extends PbnMsg
  case object ToggleOutlines                               extends PbnMsg
  case object DownloadPdf                                 extends PbnMsg
  case object StartOver                                   extends PbnMsg
  case object NoOp                                        extends PbnMsg
}

object PaintByNumbersScreen extends Screen {

  type Model = PbnModel
  type Msg   = PbnMsg

  sealed trait Phase
  object Phase {
    case object Upload                    extends Phase
    case object Processing                extends Phase
    case object Preview                   extends Phase
    case class Error(message: String)     extends Phase
  }

  final case class PbnModel(
      phase: Phase,
      colorCount: Int,
      imageDataUrl: Option[String],
      result: Option[PbnResult],
      showOutlines: Boolean
  )

  private val FileInputId = "pbn-file-input"

  def init: (PbnModel, Cmd[IO, PbnMsg]) =
    (PbnModel(Phase.Upload, 12, None, None, true), Cmd.None)

  def update(model: PbnModel): PbnMsg => (PbnModel, Cmd[IO, PbnMsg]) = {
    case PbnMsg.FileInputChanged =>
      val cmd = Cmd.Run[IO, PbnMsg, PbnMsg](
        IO.async_[PbnMsg] { cb =>
          Option(dom.document.getElementById(FileInputId))
            .map(_.asInstanceOf[dom.HTMLInputElement])
            .filter(_.files.length > 0) match {
            case Some(el) =>
              ImageLoader.readFileAsDataUrl(
                el.files(0),
                dataUrl => cb(Right(PbnMsg.FileSelected(dataUrl)))
              )
            case None => cb(Right(PbnMsg.NoOp))
          }
        }
      )(identity)
      (model, cmd)

    case PbnMsg.FileSelected(dataUrl) =>
      (model.copy(imageDataUrl = Some(dataUrl)), Cmd.None)

    case PbnMsg.SetColorCount(n) =>
      (model.copy(colorCount = Math.max(2, Math.min(30, n))), Cmd.None)

    case PbnMsg.Generate =>
      model.imageDataUrl match {
        case None => (model, Cmd.None)
        case Some(dataUrl) =>
          val cmd = Cmd.Run[IO, PbnMsg, PbnMsg](
            IO.async_[PixelGrid] { cb =>
              ImageLoader.loadFromDataUrl(
                dataUrl,
                grid => cb(Right(grid)),
                err => cb(Left(new Exception(err)))
              )
            }.flatMap { grid =>
              IO(processImage(grid, model.colorCount))
            }.attempt.map {
              case Right(result) => PbnMsg.ProcessingDone(result)
              case Left(err)     => PbnMsg.ProcessingFailed(err.getMessage)
            }
          )(identity)
          (model.copy(phase = Phase.Processing), cmd)
      }

    case PbnMsg.ProcessingDone(result) =>
      (model.copy(phase = Phase.Preview, result = Some(result)), Cmd.None)

    case PbnMsg.ProcessingFailed(error) =>
      (model.copy(phase = Phase.Error(error)), Cmd.None)

    case PbnMsg.ToggleOutlines =>
      (model.copy(showOutlines = !model.showOutlines), Cmd.None)

    case PbnMsg.DownloadPdf =>
      model.result match {
        case None => (model, Cmd.None)
        case Some(result) =>
          val cmd = Cmd.Run[IO, Unit, PbnMsg](IO(generatePdf(result)))(_ => PbnMsg.NoOp)
          (model, cmd)
      }

    case PbnMsg.StartOver =>
      init

    case PbnMsg.NoOp =>
      (model, Cmd.None)
  }

  private def processImage(grid: PixelGrid, colorCount: Int): PbnResult = {
    val palette   = MedianCut.quantize(grid, colorCount)
    val regionMap = RegionDetector.detect(grid, palette)
    PbnRenderer.render(regionMap)
  }

  def view(model: PbnModel): Html[PbnMsg] =
    div(`class` := "screen-container")(
      div(`class` := "screen-container-inner")(
        h1(`class` := "app-title")(text("Malen nach Zahlen")),
        p(`class` := "app-subtitle")(text("Upload a photo and get a printable paint-by-numbers PDF")),
        model.phase match {
          case Phase.Upload       => uploadView(model)
          case Phase.Processing   => processingView
          case Phase.Preview      => previewView(model)
          case Phase.Error(msg)   => errorView(msg)
        }
      )
    )

  private def uploadView(model: PbnModel): Html[PbnMsg] =
    div(`class` := "upload-section")(
      div(`class` := "upload-zone")(
        label(`class` := "upload-label", attribute("for", FileInputId))(
          model.imageDataUrl match {
            case None =>
              div(`class` := "upload-placeholder")(
                div(`class` := "upload-icon")(text("\ud83d\uddbc")),
                p()(text("Click to select an image")),
                p(`class` := "upload-hint")(text("JPG, PNG, or WebP"))
              )
            case Some(dataUrl) =>
              img(src := dataUrl, `class` := "upload-preview-img")
          }
        ),
        input(
          `type`  := "file",
          id      := FileInputId,
          `class` := "upload-file-input",
          attribute("accept", "image/*"),
          onInput(_ => PbnMsg.FileInputChanged)
        )
      ),
      div(`class` := "settings-row")(
        div(`class` := "color-count-row")(
          label(`class` := "label-block")(text(s"Number of colors: ${model.colorCount}")),
          input(
            `type` := "range",
            attribute("min", "2"),
            attribute("max", "30"),
            value  := model.colorCount.toString,
            `class` := "color-slider",
            onInput(s => PbnMsg.SetColorCount(s.toIntOption.getOrElse(12)))
          )
        )
      ),
      div(`class` := "generate-row")(
        button(
          `class` := s"is-primary generate-btn${if (model.imageDataUrl.isEmpty) " btn-disabled" else ""}",
          onClick(PbnMsg.Generate)
        )(text("Generate"))
      )
    )

  private val processingView: Html[PbnMsg] =
    div(`class` := "processing-section")(
      div(`class` := "spinner")(),
      p(`class` := "processing-text")(text("Analyzing image and creating regions...")),
      p(`class` := "processing-hint")(text("This may take a few seconds for large images."))
    )

  private def errorView(message: String): Html[PbnMsg] =
    div(`class` := "error-section")(
      p(`class` := "error-text")(text(s"Something went wrong: $message")),
      div(`class` := "action-row")(
        button(`class` := "is-primary", onClick(PbnMsg.StartOver))(text("Try Again"))
      )
    )

  private def previewView(model: PbnModel): Html[PbnMsg] =
    model.result match {
      case None => div()(text("No result"))
      case Some(result) =>
        val coloredSrc =
          if (model.showOutlines) result.coloredDataUrl
          else result.coloredFlatDataUrl
        val outlineLabel =
          if (model.showOutlines) "Hide Outlines"
          else "Show Outlines"
        div(`class` := "preview-section")(
          div(`class` := "preview-images")(
            div(`class` := "preview-card")(
              h3()(text("Paint-by-Numbers Outline")),
              img(src := result.outlineDataUrl, `class` := "preview-img")
            ),
            div(`class` := "preview-card")(
              div(`class` := "preview-card-header")(
                h3()(text("Colored Preview")),
                button(
                  `class` := "outline-toggle",
                  onClick(PbnMsg.ToggleOutlines)
                )(text(outlineLabel))
              ),
              img(src := coloredSrc, `class` := "preview-img")
            )
          ),
          div(`class` := "adjust-colors")(
            div(`class` := "adjust-colors-row")(
              label(`class` := "label-block")(text(s"Colors: ${model.colorCount}")),
              input(
                `type` := "range",
                attribute("min", "2"),
                attribute("max", "30"),
                value  := model.colorCount.toString,
                `class` := "color-slider",
                onInput(s => PbnMsg.SetColorCount(s.toIntOption.getOrElse(12)))
              ),
              button(
                `class` := "is-primary",
                onClick(PbnMsg.Generate)
              )(text("Update"))
            )
          ),
          div(`class` := "color-legend")(
            h3()(text("Color Legend")),
            div(`class` := "legend-grid")(
              result.palette.zipWithIndex.map { case (c, idx) =>
                val num = idx + 1
                div(`class` := "legend-item")(
                  div(
                    `class` := "legend-swatch",
                    style(Style("background-color" -> s"rgb(${c.r},${c.g},${c.b})"))
                  )(),
                  span(`class` := "legend-number")(text(num.toString))
                )
              }*
            )
          ),
          div(`class` := "action-row")(
            button(`class` := "is-primary", onClick(PbnMsg.DownloadPdf))(
              text("Download PDF")
            ),
            button(onClick(PbnMsg.StartOver))(text("Start Over"))
          )
        )
    }

  private def generatePdf(result: PbnResult): Unit = {
    val pageW   = 210.0
    val pageH   = 297.0
    val margin  = 15.0
    val usableW = pageW - 2 * margin
    val usableH = pageH - 2 * margin - 30

    val imgAspect = result.imageWidth.toDouble / result.imageHeight.toDouble
    val (imgW, imgH) =
      if (usableW / imgAspect <= usableH) (usableW, usableW / imgAspect)
      else (usableH * imgAspect, usableH)

    val imgX = margin + (usableW - imgW) / 2
    val imgY = margin + 20

    val legendStartY = imgY + imgH + 10
    val legendFits   = legendStartY + 40 < pageH - margin

    val titleInstr = List(
      Instruction.PageSize(pageW, pageH),
      Instruction.FontSize(16),
      Instruction.TextCentered(pageW / 2, margin + 10, "Paint by Numbers")
    )

    val imageInstr = List(
      Instruction.AddImage(result.outlineDataUrl, imgX, imgY, imgW, imgH)
    )

    val legendInstr =
      if (legendFits) legendInstructions(result, margin, legendStartY, usableW)
      else List(Instruction.AddPage) ++ legendInstructions(result, margin, margin + 15, usableW)

    val saveInstr = List(Instruction.Save("paint-by-numbers.pdf"))

    JsPDF.run(titleInstr ++ imageInstr ++ legendInstr ++ saveInstr)
  }

  private def legendInstructions(
      result: PbnResult,
      margin: Double,
      startY: Double,
      usableW: Double
  ): List[Instruction] = {
    val cols    = 6
    val swatchW = 12.0
    val swatchH = 8.0
    val cellW   = usableW / cols
    val cellH   = 14.0

    val header = List(
      Instruction.FontSize(11),
      Instruction.Text(margin, startY, "Color Legend:")
    )

    val items = result.palette.zipWithIndex.flatMap { case (c, i) =>
      val col = i % cols
      val row = i / cols
      val x   = margin + col * cellW
      val y   = startY + 6 + row * cellH
      List(
        Instruction.FillRect(x, y, swatchW, swatchH, c.r, c.g, c.b),
        Instruction.FontSize(9),
        Instruction.Text(x + swatchW + 2, y + swatchH - 1, (i + 1).toString)
      )
    }.toList

    header ++ items
  }
}
