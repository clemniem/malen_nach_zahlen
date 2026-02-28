package scaffold.common.image

final case class NumberPlacement(xPx: Double, yPx: Double, fontSizePx: Int, label: String)

final case class PbnResult(
    palette: Vector[Rgb],
    regionMap: RegionMap,
    outlineDataUrl: String,
    outlineOnlyDataUrl: String,
    numberPlacements: Vector[NumberPlacement],
    coloredDataUrl: String,
    coloredFlatDataUrl: String,
    imageWidth: Int,
    imageHeight: Int
)
