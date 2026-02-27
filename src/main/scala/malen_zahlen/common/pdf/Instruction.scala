package scaffold.common.pdf

sealed trait Instruction

object Instruction {
  case class PageSize(widthMm: Double, heightMm: Double) extends Instruction
  case class FontSize(pt: Int)                           extends Instruction
  case class Text(xMm: Double, yMm: Double, value: String) extends Instruction
  case object AddPage                                    extends Instruction
  case class FillRect(xMm: Double, yMm: Double, widthMm: Double, heightMm: Double, r: Int, g: Int, b: Int)
    extends Instruction
  case class Save(filename: String) extends Instruction
}
