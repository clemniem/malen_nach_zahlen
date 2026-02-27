package scaffold.common.image

final case class PbnResult(
    palette: Vector[Rgb],
    regionMap: RegionMap,
    outlineDataUrl: String,
    coloredDataUrl: String,
    coloredFlatDataUrl: String,
    imageWidth: Int,
    imageHeight: Int
)
