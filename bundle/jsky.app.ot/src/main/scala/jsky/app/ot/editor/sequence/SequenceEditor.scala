package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.ISPSequence
import edu.gemini.spModel.sequence.{Metadata, F2, InstrumentSequence, F2Sequence, Describe, Sequence}
import edu.gemini.spModel.sequence.sp.SequenceDO
import jsky.app.ot.editor.OtItemEditor

import java.awt.Font
import java.beans.PropertyEditor
import javax.swing.{JPanel, BorderFactory}

import scala.swing.{Insets, GridBagPanel, Label}
import scala.swing.GridBagPanel.Anchor
import scala.swing.GridBagPanel.Fill

import scalaz._
import Scalaz._

object SequenceEditor {
  var NewSequenceSupport = false

  private trait Row {
    def layout(pan: GridBagPanel, row: Int, col: Int): Unit
  }

  private case class TitleRow(label: Metadata.Label) extends Row {
    def layout(pan: GridBagPanel, row: Int, col: Int): Unit = {
      val lab = new Label(label.shows)
      val fnt = lab.font
      lab.font = fnt.deriveFont(Font.BOLD | Font.ITALIC, fnt.getSize2D + 4.0f)
      pan.layout(lab) = new pan.Constraints {
        gridx     = col
        gridy     = row
        anchor    = Anchor.West
        insets    = new Insets(10, 0, 0, 0)
        gridwidth = 3
      }
    }
  }

  private case class PropRow(ed: PropEditor) extends Row {
    def layout(pan: GridBagPanel, row: Int, col: Int): Unit = {
      pan.layout(ed.label) = new pan.Constraints {
        gridx  = col
        gridy  = row
        anchor = Anchor.East
        insets = new Insets(5, 10, 0, 5)
      }
      pan.layout(ed.component) = new pan.Constraints {
        gridx  = col + 1
        gridy  = row
        fill   = Fill.Horizontal
        insets = new Insets(5, 0, 0, 0)
      }
      ed.units.foreach { units =>
        pan.layout(units) = new pan.Constraints {
          gridx  = col + 2
          gridy  = row
          anchor = Anchor.West
          insets = new Insets(5, 5, 0, 0)
        }
      }
    }
  }
}

class SequenceEditor extends OtItemEditor[ISPSequence, SequenceDO] {

  import jsky.app.ot.editor.sequence.SequenceEditor.{Row, PropRow, TitleRow}

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

    val rows = sections.list.flatMap { s =>
      val title = s.props.headOption.flatMap(_.meta.attrs.label.parent).map(TitleRow)
      val props = s.editors.map(PropRow)

      title.fold(props: List[Row]) { _ :: props }
    }

    rows.zipWithIndex.foreach { case (row, i) =>
        row.layout(pan, i, 0)
    }
  }

  override def getWindow: JPanel = pan.peer
}
