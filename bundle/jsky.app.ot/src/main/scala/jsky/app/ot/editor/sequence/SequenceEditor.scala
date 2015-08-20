package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.ISPSequence
import edu.gemini.spModel.sequence.InstrumentSequence
import edu.gemini.spModel.sequence.sp.SequenceDO

import jsky.app.ot.editor.OtItemEditor

import javax.swing.JPanel
import scala.swing.BorderPanel

import scalaz._
import Scalaz._

object SequenceEditor {
  var NewSequenceSupport = false
}

class SequenceEditor extends OtItemEditor[ISPSequence, SequenceDO] {

  val pan = new BorderPanel

  private def commit(is: InstrumentSequence): Unit = {
    val dob = getDataObject
    dob.seq = Some(is)
    apply()
  }

  override protected def init(): Unit = {
    val ps = new PropSheet(getDataObject.seq, Set(0), commit)
    pan.layout(ps) = BorderPanel.Position.South
  }

  override def getWindow: JPanel = pan.peer
}
