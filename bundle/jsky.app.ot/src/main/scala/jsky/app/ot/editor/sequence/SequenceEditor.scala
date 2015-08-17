package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.ISPSequence
import edu.gemini.spModel.sequence.{F2, InstrumentSequence, F2Sequence, Describe, Sequence}
import edu.gemini.spModel.sequence.sp.SequenceDO
import jsky.app.ot.editor.OtItemEditor

import javax.swing.{JPanel, BorderFactory}

import scala.swing.{Insets, GridBagPanel}
import scala.swing.GridBagPanel.Anchor
import scala.swing.GridBagPanel.Fill

object SequenceEditor {
  var NewSequenceSupport = false
}

class SequenceEditor extends OtItemEditor[ISPSequence, SequenceDO] {
  val pan = new GridBagPanel {
    border = BorderFactory.createCompoundBorder(
      BorderFactory.createEmptyBorder(10, 10, 10, 10),
      BorderFactory.createLoweredSoftBevelBorder())
  }

  override protected def init(): Unit = {
    pan.layout.clear()
    getDataObject.seq.foreach {
      case F2Sequence(s) => init(s, (s: Sequence[F2]) => F2Sequence(s))
      case _             => // do nothing
    }
  }

  private def init[I: Describe](s: Sequence[I], ctor: Sequence[I] => InstrumentSequence): Unit = {
    val sections = PropSection.sections(s, Set(0), (seq: Sequence[I]) => {
      val dob = getDataObject
      dob.seq = Some(ctor(seq))
      apply()
    })

    val editors  = sections.list.flatMap { ps => ps.editors }.zipWithIndex

    editors.foreach { case (ed, row) =>
      pan.layout(ed.label) = new pan.Constraints {
        gridx  = 0
        gridy  = row
        anchor = Anchor.East
        insets = new Insets(5, 0, 0, 5)
      }
      pan.layout(ed.component) = new pan.Constraints {
        gridx  = 1
        gridy  = row
        fill   = Fill.Horizontal
        insets = new Insets(5, 0, 0, 0)
      }
      ed.units.foreach { units =>
        pan.layout(units) = new pan.Constraints {
          gridx  = 2
          gridy  = row
          anchor = Anchor.West
          insets = new Insets(5, 5, 0, 0)
        }
      }
    }

  }

  override def getWindow: JPanel = pan.peer
}
