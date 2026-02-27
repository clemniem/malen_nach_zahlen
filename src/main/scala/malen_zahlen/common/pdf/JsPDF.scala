package scaffold.common.pdf

import org.scalajs.dom.console
import scala.scalajs.js

object JsPDF {

  def run(instructions: List[Instruction]): Unit = {
    val _ = js.timers.setTimeout(0)(runNow(instructions))
  }

  def runNow(instructions: List[Instruction]): Unit =
    getJsPDFConstructor match {
      case None =>
        console.warn("JsPDF: jsPDF not found on window. Add script tag for jspdf.umd.min.js.")
      case Some(ctor) =>
        type State = (Option[js.Dynamic], (Double, Double))
        val _ = instructions.foldLeft[State]((Option.empty[js.Dynamic], (0.0, 0.0))) { (state, inst) =>
          val (docOpt, (pageW, pageH)) = state
          inst match {
            case Instruction.PageSize(w, h) =>
              val opts = js.Dynamic.literal(orientation = "p", unit = "mm", format = js.Array(w, h))
              val doc  = js.Dynamic.newInstance(ctor)(opts)
              (Some(doc), (w, h))
            case Instruction.FontSize(pt) =>
              docOpt.foreach(doc => { val _ = doc.setFontSize(pt) })
              (docOpt, (pageW, pageH))
            case Instruction.Text(x, y, value) =>
              docOpt.foreach(doc => { val _ = doc.text(value, x, y) })
              (docOpt, (pageW, pageH))
            case Instruction.AddPage =>
              docOpt.foreach(doc => { val _ = doc.addPage() })
              (docOpt, (pageW, pageH))
            case Instruction.FillRect(x, y, w, h, r, g, b) =>
              docOpt.foreach { doc =>
                val _ = doc.setFillColor(r, g, b)
                val _ = doc.rect(x, y, w, h, "F")
              }
              (docOpt, (pageW, pageH))
            case Instruction.Save(filename) =>
              docOpt.foreach(doc => { val _ = doc.save(filename) })
              (docOpt, (pageW, pageH))
          }
        }
    }

  private def getJsPDFConstructor: Option[js.Dynamic] = {
    val jspdfObj = js.Dynamic.global.selectDynamic("jspdf")
    if (js.typeOf(jspdfObj) == "undefined") None
    else {
      val ctorKey = "js" + "PDF"
      val ctor    = jspdfObj.selectDynamic(ctorKey)
      if (js.typeOf(ctor) != "undefined") Some(ctor.asInstanceOf[js.Dynamic]) else None
    }
  }
}
