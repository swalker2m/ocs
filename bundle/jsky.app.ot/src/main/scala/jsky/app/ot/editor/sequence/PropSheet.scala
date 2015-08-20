package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence._
import jsky.app.ot.util.OtColor

import java.awt.{Color, Font}
import javax.swing.BorderFactory

import scala.swing.{TextField, CheckBox, ComboBox, Component, GridBagPanel, Insets, Label}
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.swing.event.{ValueChanged, ActionEvent, SelectionChanged}

import scalaz._
import Scalaz._

final class PropSheet(is: Option[InstrumentSequence], selected: Set[Int], commit: InstrumentSequence => Unit) extends GridBagPanel {
  border = BorderFactory.createCompoundBorder(
    BorderFactory.createLoweredSoftBevelBorder(),
    BorderFactory.createEmptyBorder(10, 10, 10, 10))

  is.foreach {
    case F2Sequence(s) => init(s, (s: Sequence[F2]) => F2Sequence(s))
    case _             => // do nothing
  }

  private final case class Title(label: Metadata.Label) {
    def layout(row: Int, col: Int): Unit = {
      val lab = new Label(label.shows)
      val fnt = lab.font
      lab.font = fnt.deriveFont(Font.BOLD | Font.ITALIC, fnt.getSize2D + 4.0f)
      PropSheet.this.layout(lab) = new Constraints {
        gridx     = col
        gridy     = row
        anchor    = Anchor.West
        insets    = new Insets(if (row == 0) 0 else 10, 0, 0, 0)
        gridwidth = 3
      }
    }
  }

  private final case class Editor(label: Label, component: Component, units: Option[Label]) {
    def layout(row: Int, col: Int): Unit = {
      PropSheet.this.layout(label) = new Constraints {
        gridx  = col
        gridy  = row
        anchor = Anchor.East
        insets = new Insets(5, if (col == 0) 10 else 20, 0, 5)
      }
      PropSheet.this.layout(component) = new Constraints {
        gridx  = col + 1
        gridy  = row
        fill   = Fill.Horizontal
        insets = new Insets(5, 0, 0, 0)
      }
      units.foreach { units =>
        PropSheet.this.layout(units) = new Constraints {
          gridx  = col + 2
          gridy  = row
          anchor = Anchor.West
          insets = new Insets(5, 5, 0, 0)
        }
      }
    }
  }

  private def init[I: Describe](seq: Sequence[I], ctor: Sequence[I] => InstrumentSequence): Unit = {
    val steps  = seq.toSteps.list.zipWithIndex.collect {
      case (step, i) if selected(i) => step
    }

    val groups = PropGroup.groups(steps)

    // Will split the properties in each section into two columns, but only if
    // there are at least 4 properties.
    def splitPos(lst: List[_]): Int = {
      val size = lst.size
      if (size < 4) size else (size / 2) + (size % 2)
    }

    // Pair each property group with the row index where it should start.
    val groupStartRow = groups.zip(groups.scanLeft(0) { (s, grp) =>
      s + 1 + splitPos(grp.props)
    })

    // Layout the properties with a title, split properties in two columns.
    groupStartRow.foreach { case (grp, row) =>
      grp.label.map(Title).foreach { _.layout(row, 0) }

      val edRows = grp.stepLenses.map { sLens =>
        val pb = sLens.getCommon(steps)
        editor(sLens)(pb, (newPb: sLens.prop.B) => {
          val curPb = sLens.getAll(seq, selected)
          if (newPb != curPb) commit(ctor(sLens.setAll(seq, selected, newPb)))
        })
      }

      val (col0, col1) = edRows.splitAt(splitPos(edRows))

      def layout(es: List[Editor], col: Int): Unit =
        es.zipWithIndex.foreach { case (e, i) => e.layout(row + i + 1, col) }

      layout(col0, 0)
      layout(col1, 3)
    }
  }

  private def editor[I: Describe](sLens: StepLens[_,I])(value: Option[sLens.prop.B], set: sLens.prop.B => Unit): Editor = {
    val lab = new Label(sLens.prop.meta.attrs.label.name)

    sLens.prop.meta match {

      case EnumMetadata(attrs, values)              =>
        val combo = new ComboBox[sLens.prop.B](values.list)

        reactions += {
          case SelectionChanged(`combo`) => set(combo.selection.item)
        }

        value.fold(combo.selection.index = -1) { pb =>
          combo.selection.item = pb
        }

        listenTo(combo.selection)
        Editor(lab, combo, None)

      case BooleanMetadata(attrs, t, f, _, _)       =>
        val check = new CheckBox()

        reactions += {
          case ActionEvent(`check`) => set(if (check.selected) t else f)
        }

        value.fold(check.selected = false) { pb =>
          check.selected = pb == t
        }

        listenTo(check)
        Editor(lab, check, None)

      case TextMetadata(attrs, us, show, read)      =>
        val field = new TextField()

        reactions += {
          case vc: ValueChanged if vc.source == field =>
            read(field.text) match {
              case -\/(_)  => field.background = OtColor.LIGHT_SALMON
              case \/-(pb) => field.background = Color.WHITE
                              set(pb)
            }
        }

        field.text = value.fold("") { pb => show(pb) }

        listenTo(field)
        Editor(lab, field, us.map(new Label(_)))
    }
  }
}



