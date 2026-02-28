package scaffold.common.image

import scala.collection.mutable.ArrayDeque

final case class RegionMap(
    width: Int,
    height: Int,
    labels: Array[Int],
    colorIndices: Array[Int],
    palette: Vector[Rgb],
    regionCount: Int
)

object RegionDetector {

  def detect(grid: PixelGrid, palette: Vector[Rgb], minRegionPixels: Int, smoothRadius: Int): RegionMap = {
    val colorIndexRaw = assignColors(grid, palette)
    val colorIndex    = if (smoothRadius <= 0) colorIndexRaw else modeFilter(colorIndexRaw, grid.width, grid.height, smoothRadius)
    val labels        = labelRegions(grid.width, grid.height, colorIndex)
    val regionCount  = if (labels.isEmpty) 0 else labels.max + 1
    mergeSmallRegions(grid.width, grid.height, labels, colorIndex, palette, regionCount, minRegionPixels)
  }

  private def modeFilter(colorIndex: Array[Int], w: Int, h: Int, radius: Int): Array[Int] = {
    val out = new Array[Int](w * h)
    val r   = radius
    (0 until h).foreach { y =>
      (0 until w).foreach { x =>
        val neighbors = for {
          dy <- (-r).max(-y) to r.min(h - 1 - y)
          dx <- (-r).max(-x) to r.min(w - 1 - x)
        } yield colorIndex((y + dy) * w + (x + dx))
        val mode = neighbors.groupBy(identity).view.mapValues(_.size).maxByOption(_._2).fold(colorIndex(y * w + x))(_._1)
        out(y * w + x) = mode
      }
    }
    out
  }

  def assignColors(grid: PixelGrid, palette: Vector[Rgb]): Array[Int] =
    Array.tabulate(grid.pixelCount)(i => nearestColor(grid.r(i), grid.g(i), grid.b(i), palette))

  def nearestColor(r: Int, g: Int, b: Int, palette: Vector[Rgb]): Int =
    palette.indices.foldLeft((0, Int.MaxValue)) { case ((best, bestDst), i) =>
      val c  = palette(i)
      val dr = r - c.r; val dg = g - c.g; val db = b - c.b
      val d  = dr * dr + dg * dg + db * db
      if (d < bestDst) (i, d) else (best, bestDst)
    }._1

  private def labelRegions(w: Int, h: Int, colorIndex: Array[Int]): Array[Int] = {
    val n      = w * h
    val labels = Array.fill(n)(-1)
    val queue  = ArrayDeque.empty[Int]
    val _ = (0 until n).foldLeft(0) { (nextId, i) =>
      if (labels(i) != -1) nextId
      else {
        labels(i) = nextId
        queue.append(i)
        drainBfs(queue, labels, nextId, colorIndex, w, h, colorIndex(i))
        nextId + 1
      }
    }
    labels
  }

  @annotation.tailrec
  private def drainBfs(
      queue: ArrayDeque[Int],
      labels: Array[Int],
      regionId: Int,
      colorIndex: Array[Int],
      w: Int,
      h: Int,
      col: Int
  ): Unit =
    if (queue.isEmpty) ()
    else {
      val idx = queue.removeHead()
      val x   = idx % w
      val y   = idx / w
      if (x > 0     && labels(idx - 1) == -1 && colorIndex(idx - 1) == col) { labels(idx - 1) = regionId; queue.append(idx - 1) }
      if (x < w - 1 && labels(idx + 1) == -1 && colorIndex(idx + 1) == col) { labels(idx + 1) = regionId; queue.append(idx + 1) }
      if (y > 0     && labels(idx - w) == -1 && colorIndex(idx - w) == col) { labels(idx - w) = regionId; queue.append(idx - w) }
      if (y < h - 1 && labels(idx + w) == -1 && colorIndex(idx + w) == col) { labels(idx + w) = regionId; queue.append(idx + w) }
      drainBfs(queue, labels, regionId, colorIndex, w, h, col)
    }

  private def mergeSmallRegions(
      w: Int,
      h: Int,
      labels: Array[Int],
      colorIndex: Array[Int],
      palette: Vector[Rgb],
      regionCount: Int,
      minRegionPixels: Int
  ): RegionMap = {
    val sizes       = new Array[Int](regionCount)
    val regionColor = new Array[Int](regionCount)
    labels.indices.foreach { i =>
      sizes(labels(i)) += 1
      regionColor(labels(i)) = colorIndex(i)
    }

    val bestNeighbor = Array.fill(regionCount)(-1)
    val edgeWeight   = new Array[Int](regionCount)

    labels.indices.foreach { i =>
      val rid = labels(i)
      if (sizes(rid) < minRegionPixels) {
        val x = i % w
        val y = i / w
        List(
          Option.when(x > 0)(labels(i - 1)),
          Option.when(x < w - 1)(labels(i + 1)),
          Option.when(y > 0)(labels(i - w)),
          Option.when(y < h - 1)(labels(i + w))
        ).flatten.filter(_ != rid).foreach { nid =>
          val weight = sizes(nid)
          if (weight > edgeWeight(rid)) {
            edgeWeight(rid) = weight
            bestNeighbor(rid) = nid
          }
        }
      }
    }

    val mergeTarget = Array.tabulate(regionCount) { rid =>
      if (bestNeighbor(rid) >= 0) bestNeighbor(rid) else rid
    }

    (0 until regionCount).foreach { i =>
      mergeTarget(i) = resolve(mergeTarget, i, regionCount)
    }

    labels.indices.foreach { i =>
      labels(i) = mergeTarget(labels(i))
      colorIndex(i) = regionColor(mergeTarget(labels(i)))
    }

    val usedIds    = labels.toSet.toVector.sorted
    val remap      = usedIds.zipWithIndex.toMap
    val finalCount = usedIds.size
    labels.indices.foreach(i => labels(i) = remap(labels(i)))

    val finalRegionColor = new Array[Int](finalCount)
    labels.indices.foreach(i => finalRegionColor(labels(i)) = colorIndex(i))

    RegionMap(w, h, labels, finalRegionColor, palette, finalCount)
  }

  @annotation.tailrec
  private def resolve(mergeTarget: Array[Int], i: Int, maxSteps: Int): Int = {
    val t = mergeTarget(i)
    if (t == i || maxSteps <= 0) i else resolve(mergeTarget, t, maxSteps - 1)
  }
}
