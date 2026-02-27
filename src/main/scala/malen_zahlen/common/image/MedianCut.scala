package scaffold.common.image

final case class Rgb(r: Int, g: Int, b: Int)

object MedianCut {

  def quantize(grid: PixelGrid, numColors: Int): Vector[Rgb] = {
    val indices = Vector.tabulate(grid.pixelCount)(identity)
    if (indices.isEmpty) Vector.empty
    else {
      val boxes = split(grid, Vector(indices), numColors)
      boxes.map(box => averageColor(grid, box))
    }
  }

  @annotation.tailrec
  private def split(grid: PixelGrid, boxes: Vector[Vector[Int]], target: Int): Vector[Vector[Int]] =
    if (boxes.size >= target) boxes
    else {
      val idx    = boxes.indices.maxBy(i => channelRange(grid, boxes(i)))
      val box    = boxes(idx)
      val ch     = widestChannel(grid, box)
      val sorted = box.sortBy(i => channelValue(grid, i, ch))
      val mid    = sorted.size / 2
      val left   = sorted.take(mid)
      val right  = sorted.drop(mid)
      if (left.isEmpty || right.isEmpty) boxes
      else split(grid, boxes.patch(idx, Vector(left, right), 1), target)
    }

  private def channelValue(grid: PixelGrid, pixelIdx: Int, ch: Int): Int =
    if (ch == 0) grid.r(pixelIdx)
    else if (ch == 1) grid.g(pixelIdx)
    else grid.b(pixelIdx)

  private case class RangeAcc(
      rMin: Int, rMax: Int,
      gMin: Int, gMax: Int,
      bMin: Int, bMax: Int
  )

  private def computeRange(grid: PixelGrid, box: Vector[Int]): RangeAcc =
    box.foldLeft(RangeAcc(255, 0, 255, 0, 255, 0)) { (acc, i) =>
      val r = grid.r(i); val g = grid.g(i); val b = grid.b(i)
      RangeAcc(
        Math.min(acc.rMin, r), Math.max(acc.rMax, r),
        Math.min(acc.gMin, g), Math.max(acc.gMax, g),
        Math.min(acc.bMin, b), Math.max(acc.bMax, b)
      )
    }

  private def channelRange(grid: PixelGrid, box: Vector[Int]): Int = {
    val r = computeRange(grid, box)
    Math.max(r.rMax - r.rMin, Math.max(r.gMax - r.gMin, r.bMax - r.bMin))
  }

  private def widestChannel(grid: PixelGrid, box: Vector[Int]): Int = {
    val r      = computeRange(grid, box)
    val rRange = r.rMax - r.rMin
    val gRange = r.gMax - r.gMin
    val bRange = r.bMax - r.bMin
    if (rRange >= gRange && rRange >= bRange) 0
    else if (gRange >= bRange) 1
    else 2
  }

  private def averageColor(grid: PixelGrid, box: Vector[Int]): Rgb = {
    val (rSum, gSum, bSum) = box.foldLeft((0L, 0L, 0L)) { case ((rs, gs, bs), i) =>
      (rs + grid.r(i), gs + grid.g(i), bs + grid.b(i))
    }
    val n = box.size
    Rgb((rSum / n).toInt, (gSum / n).toInt, (bSum / n).toInt)
  }
}
