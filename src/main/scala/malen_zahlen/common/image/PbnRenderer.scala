package scaffold.common.image

import org.scalajs.dom.{document, HTMLCanvasElement, CanvasRenderingContext2D}
import scala.scalajs.js.typedarray.Uint8ClampedArray

object PbnRenderer {

  def render(regionMap: RegionMap): PbnResult = {
    val outlineUrl      = renderOutline(regionMap)
    val outlineOnlyUrl  = renderOutlineOnly(regionMap)
    val placements      = computeNumberPlacements(regionMap)
    val coloredUrl      = renderColored(regionMap, drawOutlines = true)
    val coloredFlatUrl  = renderColored(regionMap, drawOutlines = false)
    PbnResult(
      regionMap.palette,
      regionMap,
      outlineUrl,
      outlineOnlyUrl,
      placements,
      coloredUrl,
      coloredFlatUrl,
      regionMap.width,
      regionMap.height
    )
  }

  private def renderOutline(rm: RegionMap): String = {
    val canvas = createOutlineCanvas(rm)
    val ctx    = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    drawNumbers(ctx, rm)
    canvas.toDataURL("image/png")
  }

  private def renderOutlineOnly(rm: RegionMap): String =
    createOutlineCanvas(rm).toDataURL("image/png")

  private def createOutlineCanvas(rm: RegionMap): HTMLCanvasElement = {
    val w      = rm.width
    val h      = rm.height
    val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.width = w
    canvas.height = h
    val ctx     = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val imgData = ctx.createImageData(w, h)
    val data    = imgData.data.asInstanceOf[Uint8ClampedArray]
    (0 until w * h).foreach { i =>
      val x        = i % w
      val y        = i / w
      val isBorder = isOutlinePixel(x, y, w, h, rm.labels)
      val off      = i * 4
      if (isBorder) {
        data(off) = 0; data(off + 1) = 0; data(off + 2) = 0; data(off + 3) = 255
      } else {
        data(off) = 255; data(off + 1) = 255; data(off + 2) = 255; data(off + 3) = 255
      }
    }
    ctx.putImageData(imgData, 0, 0)
    canvas
  }

  private def computeNumberPlacements(rm: RegionMap): Vector[NumberPlacement] = {
    val centroids     = computeCentroids(rm)
    val minDistToEdge = computeMinDistanceToBoundary(rm, centroids)
    centroids.toVector.flatMap { case (regionId, (cx, cy)) =>
      val safeRadius = minDistToEdge.getOrElse(regionId, 0.0)
      val fontSize   = fontSizeToFitInRegion(safeRadius)
      if (fontSize >= 6) {
        val colorIdx = rm.colorIndices(regionId)
        val label    = (colorIdx + 1).toString
        Some(NumberPlacement(cx, cy, fontSize, label))
      } else None
    }
  }

  private def renderColored(rm: RegionMap, drawOutlines: Boolean): String = {
    val w      = rm.width
    val h      = rm.height
    val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.width = w
    canvas.height = h
    val ctx     = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val imgData = ctx.createImageData(w, h)
    val data    = imgData.data.asInstanceOf[Uint8ClampedArray]

    (0 until w * h).foreach { i =>
      val off      = i * 4
      val isBorder = drawOutlines && isOutlinePixel(i % w, i / w, w, h, rm.labels)
      if (isBorder) {
        data(off) = 40; data(off + 1) = 40; data(off + 2) = 40; data(off + 3) = 255
      } else {
        val colorIdx = rm.colorIndices(rm.labels(i))
        val c        = rm.palette(colorIdx)
        data(off) = c.r; data(off + 1) = c.g; data(off + 2) = c.b; data(off + 3) = 255
      }
    }
    ctx.putImageData(imgData, 0, 0)
    canvas.toDataURL("image/png")
  }

  private def isOutlinePixel(x: Int, y: Int, w: Int, h: Int, labels: Array[Int]): Boolean = {
    val myLabel = labels(y * w + x)
    (x > 0 && labels(y * w + (x - 1)) != myLabel) ||
    (x < w - 1 && labels(y * w + (x + 1)) != myLabel) ||
    (y > 0 && labels((y - 1) * w + x) != myLabel) ||
    (y < h - 1 && labels((y + 1) * w + x) != myLabel)
  }

  private def drawNumbers(ctx: CanvasRenderingContext2D, rm: RegionMap): Unit = {
    val centroids     = computeCentroids(rm)
    val minDistToEdge = computeMinDistanceToBoundary(rm, centroids)

    centroids.foreach { case (regionId, (cx, cy)) =>
      val safeRadius = minDistToEdge.getOrElse(regionId, 0.0)
      val fontSize   = fontSizeToFitInRegion(safeRadius)
      if (fontSize >= 6) {
        val colorIdx = rm.colorIndices(regionId)
        val label    = (colorIdx + 1).toString
        ctx.font = s"bold ${fontSize}px sans-serif"
        ctx.textAlign = "center"
        ctx.textBaseline = "middle"
        ctx.fillStyle = "white"
        ctx.fillText(label, cx - 1, cy)
        ctx.fillText(label, cx + 1, cy)
        ctx.fillText(label, cx, cy - 1)
        ctx.fillText(label, cx, cy + 1)
        ctx.fillStyle = "black"
        ctx.fillText(label, cx, cy)
      }
    }
  }

  private def fontSizeToFitInRegion(minDistToBoundary: Double): Int = {
    if (minDistToBoundary <= 2) 0
    else {
      val size = (minDistToBoundary * 0.85).toInt
      Math.max(6, Math.min(14, size))
    }
  }

  private def computeMinDistanceToBoundary(rm: RegionMap, centroids: Map[Int, (Double, Double)]): Map[Int, Double] = {
    val w       = rm.width
    val h       = rm.height
    val minDist = Array.fill(rm.regionCount)(Double.MaxValue)
    (0 until h).foreach { y =>
      (0 until w).foreach { x =>
        val i   = y * w + x
        val rid = rm.labels(i)
        if (isOutlinePixel(x, y, w, h, rm.labels)) {
          centroids.get(rid).foreach { case (cx, cy) =>
            val d = Math.sqrt((x - cx) * (x - cx) + (y - cy) * (y - cy))
            if (d < minDist(rid)) minDist(rid) = d
          }
        }
      }
    }
    minDist.indices
      .filter(rid => minDist(rid) < Double.MaxValue)
      .map(rid => rid -> minDist(rid))
      .toMap
  }

  private def computeCentroids(rm: RegionMap): Map[Int, (Double, Double)] = {
    val init = Array.fill(rm.regionCount)((0L, 0L, 0))
    rm.labels.indices.foreach { i =>
      val rid          = rm.labels(i)
      val (xs, ys, ct) = init(rid)
      init(rid) = (xs + (i % rm.width), ys + (i / rm.width), ct + 1)
    }
    init.indices.filter(init(_)._3 > 0).map { rid =>
      val (xs, ys, ct) = init(rid)
      rid -> (xs.toDouble / ct, ys.toDouble / ct)
    }.toMap
  }
}
