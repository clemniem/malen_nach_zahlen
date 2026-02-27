package scaffold.common.image

import org.scalajs.dom
import org.scalajs.dom.{document, FileReader, HTMLCanvasElement, HTMLImageElement, CanvasRenderingContext2D}
import scala.scalajs.js.typedarray.Uint8ClampedArray

final case class PixelGrid(
    width: Int,
    height: Int,
    data: Array[Int]
) {
  def r(idx: Int): Int = data(idx * 3)
  def g(idx: Int): Int = data(idx * 3 + 1)
  def b(idx: Int): Int = data(idx * 3 + 2)
  def pixelCount: Int  = width * height
}

object ImageLoader {

  private val MaxDimension = 500

  def loadFromDataUrl(
      dataUrl: String,
      onDone: PixelGrid => Unit,
      onError: String => Unit
  ): Unit = {
    val img = document.createElement("img").asInstanceOf[HTMLImageElement]
    img.onload = { (_: dom.Event) =>
      val (w, h) = downsampleDims(img.naturalWidth, img.naturalHeight)
      val canvas  = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
      canvas.width = w
      canvas.height = h
      val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      ctx.drawImage(img, 0, 0, w, h)
      val imageData = ctx.getImageData(0, 0, w, h)
      val rgba      = imageData.data.asInstanceOf[Uint8ClampedArray]
      val pixelCount = w * h
      val rgb = Array.tabulate(pixelCount * 3) { j =>
        val pixel = j / 3
        val ch    = j % 3
        rgba(pixel * 4 + ch).toInt
      }
      onDone(PixelGrid(w, h, rgb))
    }
    img.addEventListener("error", { (_: dom.Event) =>
      onError("Failed to load image")
    })
    img.src = dataUrl
  }

  def readFileAsDataUrl(file: dom.File, onDone: String => Unit): Unit = {
    val reader = new FileReader()
    reader.onload = { (_: dom.Event) =>
      onDone(reader.result.asInstanceOf[String])
    }
    reader.readAsDataURL(file)
  }

  private def downsampleDims(origW: Int, origH: Int): (Int, Int) = {
    val maxSide = Math.max(origW, origH)
    if (maxSide <= MaxDimension) (origW, origH)
    else {
      val scale = MaxDimension.toDouble / maxSide.toDouble
      ((origW * scale).toInt, (origH * scale).toInt)
    }
  }
}
