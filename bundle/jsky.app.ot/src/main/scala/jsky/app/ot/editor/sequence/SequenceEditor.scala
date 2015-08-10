package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.ISPSequence
import edu.gemini.spModel.sequence.sp.SequenceDO
import jsky.app.ot.editor.OtItemEditor

import javax.swing.{JPanel, BorderFactory}

import scala.swing.{Label, GridBagPanel}

object SequenceEditor {
  var NewSequenceSupport = false
}

class SequenceEditor extends OtItemEditor[ISPSequence, SequenceDO] {
//  border = BorderFactory.createEmptyBorder(10, 10, 10, 10)
  override protected def init(): Unit = ()

  override def getWindow: JPanel = {
    new GridBagPanel {
      border = BorderFactory.createEmptyBorder(10, 10, 10, 10)

      layout(new Label("F2 Sequence")) = new Constraints {

      }
    }.peer
  }
}
