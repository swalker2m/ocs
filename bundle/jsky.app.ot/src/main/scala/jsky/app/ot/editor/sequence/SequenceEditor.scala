package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.ISPSequence
import edu.gemini.spModel.sequence.{Metadata, F2, InstrumentSequence, F2Sequence, Describe, Sequence}
import edu.gemini.spModel.sequence.sp.SequenceDO
import jsky.app.ot.editor.OtItemEditor

import java.awt.Font
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
        insets = new Insets(5, if (col == 0) 10 else 20, 0, 5)
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

  import jsky.app.ot.editor.sequence.SequenceEditor.{PropRow, TitleRow}

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
    // All the property sections (one for the instrument, one for telescope, etc.
    // according to the contents of the sequence).
    val sections = PropSection.sections(s, Set(0), (seq: Sequence[I]) => {
      val dob = getDataObject
      dob.seq = Some(ctor(seq))
      apply()
    })

    // Will split the properties in each section into two columns, but only if
    // there are at least 4 properties.
    def splitPos(ps: PropSection[_, I]): Int = {
      val size = ps.props.size
      if (size < 4) size else (size / 2) + (size % 2)
    }

    // Pair each property section with the row index where it should start.
    val sectionStartRow = sections.list.zip(sections.list.scanLeft(0) { (s, ps) =>
      s + 1 + splitPos(ps)
    })

    // Layout the properties with a title, split properties in two columns.
    sectionStartRow.foreach { case (ps, row) =>
      ps.props.headOption.flatMap(_.meta.attrs.label.parent).map(TitleRow).foreach {
        _.layout(pan, row, 0)
      }

      val propRows     = ps.editors.map(PropRow)
      val (col0, col1) = propRows.splitAt(splitPos(ps))

      def layout(props: List[PropRow], col: Int): Unit =
        props.zipWithIndex.foreach { case (r,i) => r.layout(pan, row + i + 1, col) }

      layout(col0, 0)
      layout(col1, 3)
    }
  }

  override def getWindow: JPanel = pan.peer
}
