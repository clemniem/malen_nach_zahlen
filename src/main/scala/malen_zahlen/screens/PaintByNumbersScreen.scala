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
  case class SetDetailLevel(n: Int)                       extends PbnMsg
  case class SetSmoothLevel(n: Int)                       extends PbnMsg
  case object Generate                                    extends PbnMsg
  case class ProcessingDone(result: PbnResult)            extends PbnMsg
  case class ProcessingFailed(error: String)              extends PbnMsg
  case object ToggleOutlines                              extends PbnMsg
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
      detailLevel: Int,
      smoothLevel: Int,
      imageDataUrl: Option[String],
      result: Option[PbnResult],
      showOutlines: Boolean
  )

  private val FileInputId = "pbn-file-input"

  private def minRegionPixelsFor(detailLevel: Int, pixelCount: Int): Int = {
    val fraction = (detailLevel * detailLevel) / 5000.0
    val minPx   = (pixelCount * fraction).toInt
    Math.max(20, Math.min(pixelCount / 4, minPx))
  }

  def init: (PbnModel, Cmd[IO, PbnMsg]) =
    (PbnModel(Phase.Upload, 12, 5, 2, None, None, true), Cmd.None)

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

    case PbnMsg.SetDetailLevel(n) =>
      (model.copy(detailLevel = Math.max(1, Math.min(10, n))), Cmd.None)

    case PbnMsg.SetSmoothLevel(n) =>
      (model.copy(smoothLevel = Math.max(0, Math.min(5, n))), Cmd.None)

    case PbnMsg.Generate =>
      model.imageDataUrl match {
        case None => (model, Cmd.None)
        case Some(dataUrl) =>
          val detail  = model.detailLevel
          val colors  = model.colorCount
          val smooth  = model.smoothLevel
          val cmd = Cmd.Run[IO, PbnMsg, PbnMsg](
            IO.async_[PixelGrid] { cb =>
              ImageLoader.loadFromDataUrl(
                dataUrl,
                grid => cb(Right(grid)),
                err => cb(Left(new Exception(err)))
              )
            }.flatMap { grid =>
              IO(processImage(grid, colors, detail, smooth))
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

  private def processImage(grid: PixelGrid, colorCount: Int, detailLevel: Int, smoothLevel: Int): PbnResult = {
    val minPx     = minRegionPixelsFor(detailLevel, grid.pixelCount)
    val palette   = MedianCut.quantize(grid, colorCount)
    val regionMap = RegionDetector.detect(grid, palette, minPx, smoothLevel)
    PbnRenderer.render(regionMap)
  }

  private def parseSlider(s: String, minVal: Int, maxVal: Int, default: Int): Int =
    s.trim.toIntOption.fold(default)(v => Math.max(minVal, Math.min(maxVal, v)))

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

  private def detailSlider(model: PbnModel): Html[PbnMsg] =
    div(`class` := "color-count-row")(
      label(`class` := "label-block")(text(s"Region size: ${model.detailLevel} (1=small, 10=large)")),
      div(`class` := "slider-with-labels")(
        span(`class` := "slider-label-left")(text("Small regions")),
        input(
          `type` := "range",
          attribute("min", "1"),
          attribute("max", "10"),
          value  := model.detailLevel.toString,
          `class` := "color-slider",
          onInput(s => PbnMsg.SetDetailLevel(parseSlider(s, 1, 10, 5)))
        ),
        span(`class` := "slider-label-right")(text("Large regions"))
      )
    )

  private def smoothSlider(model: PbnModel): Html[PbnMsg] =
    div(`class` := "color-count-row")(
      label(`class` := "label-block")(text(s"Edge smoothing: ${model.smoothLevel} (0=none, 5=max)")),
      div(`class` := "slider-with-labels")(
        span(`class` := "slider-label-left")(text("None")),
        input(
          `type` := "range",
          attribute("min", "0"),
          attribute("max", "5"),
          value  := model.smoothLevel.toString,
          `class` := "color-slider",
          onInput(s => PbnMsg.SetSmoothLevel(parseSlider(s, 0, 5, 2)))
        ),
        span(`class` := "slider-label-right")(text("Max"))
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
            onInput(s => PbnMsg.SetColorCount(parseSlider(s, 2, 30, 12)))
          )
        ),
        detailSlider(model),
        smoothSlider(model)
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
                onInput(s => PbnMsg.SetColorCount(parseSlider(s, 2, 30, 12)))
              )
            ),
            div(`class` := "adjust-colors-row")(
              label(`class` := "label-block")(text(s"Region size: ${model.detailLevel} (1=small, 10=large)")),
              div(`class` := "slider-with-labels")(
                span(`class` := "slider-label-left")(text("Small regions")),
                input(
                  `type` := "range",
                  attribute("min", "1"),
                  attribute("max", "10"),
                  value  := model.detailLevel.toString,
                  `class` := "color-slider",
                  onInput(s => PbnMsg.SetDetailLevel(parseSlider(s, 1, 10, 5)))
                ),
                span(`class` := "slider-label-right")(text("Large regions"))
              )
            ),
            div(`class` := "adjust-colors-row")(
              label(`class` := "label-block")(text(s"Edge smoothing: ${model.smoothLevel} (0=none, 5=max)")),
              div(`class` := "slider-with-labels")(
                span(`class` := "slider-label-left")(text("None")),
                input(
                  `type` := "range",
                  attribute("min", "0"),
                  attribute("max", "5"),
                  value  := model.smoothLevel.toString,
                  `class` := "color-slider",
                  onInput(s => PbnMsg.SetSmoothLevel(parseSlider(s, 0, 5, 2)))
                ),
                span(`class` := "slider-label-right")(text("Max"))
              )
            ),
            div(`class` := "adjust-colors-row")(
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
    val pageW = 210.0
    val pageH = 297.0

    val page1Margin = 18.0
    val page1UsableW = pageW - 2 * page1Margin

    val page1Instr = List(
      Instruction.PageSize(pageW, pageH),
      Instruction.FontSize(22),
      Instruction.TextCentered(pageW / 2, page1Margin + 8, "Paint by Numbers"),
      Instruction.FontSize(11),
      Instruction.Text(page1Margin, page1Margin + 22, "Instructions:"),
      Instruction.Text(page1Margin, page1Margin + 30, "1. Use the color legend below to match each number to its color."),
      Instruction.Text(page1Margin, page1Margin + 38, "2. On the next page, fill each numbered region with the matching color."),
      Instruction.Text(page1Margin, page1Margin + 46, "3. Use coloured pencils, markers, or paint to complete your picture.")
    ) ++ legendInstructions(result, page1Margin, page1Margin + 58, page1UsableW)

    val addPage2 = List(Instruction.AddPage)

    val imgAspect = result.imageWidth.toDouble / result.imageHeight.toDouble
    val (imgW, imgH) =
      if (pageW / imgAspect <= pageH) (pageW, pageW / imgAspect)
      else (pageH * imgAspect, pageH)
    val imgX = (pageW - imgW) / 2
    val imgY = (pageH - imgH) / 2

    val imageInstr = List(
      Instruction.AddImage(result.outlineOnlyDataUrl, imgX, imgY, imgW, imgH)
    )

    val ptsPerMm   = 72.0 / 25.4
    val mmPerPt    = 25.4 / 72.0
    val numberInstr = result.numberPlacements.flatMap { p =>
      val xMm = imgX + (p.xPx / result.imageWidth) * imgW
      val yMm = imgY + (p.yPx / result.imageHeight) * imgH
      val pt  = Math.max(6, Math.min(14, (p.fontSizePx * imgH / result.imageHeight * ptsPerMm).round.toInt))
      val baselineOffsetMm = pt * mmPerPt * 0.35
      List(Instruction.FontSize(pt), Instruction.TextCentered(xMm, yMm + baselineOffsetMm, p.label))
    }.toList

    val saveInstr = List(Instruction.Save("paint-by-numbers.pdf"))

    JsPDF.run(page1Instr ++ addPage2 ++ imageInstr ++ numberInstr ++ saveInstr)
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
