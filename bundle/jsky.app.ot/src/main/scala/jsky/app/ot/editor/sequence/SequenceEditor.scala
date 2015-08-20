package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.{SPNodeKey, ISPSequence}
import edu.gemini.spModel.rich.pot.sp.SpNodeKeyEqual
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

  override protected def init(): Unit = {
    def curKey: SPNodeKey = getContextObservation.getNodeKey

    // Text widgets fire value changed events when they lose focus.  If you
    // click on a different sequence node, commit will be called with the
    // provided sequence which will be for the wrong node.
    val editKey = curKey

    def commit(is: InstrumentSequence): Unit =
      if (editKey === curKey) {
        val dob = getDataObject
        dob.seq = Some(is)
        apply()
      }

    val ps = new PropSheet(getDataObject.seq, Set(0), commit)

    pan.layout.clear()
    pan.layout(ps) = BorderPanel.Position.South
  }

  override def getWindow: JPanel = pan.peer
}
