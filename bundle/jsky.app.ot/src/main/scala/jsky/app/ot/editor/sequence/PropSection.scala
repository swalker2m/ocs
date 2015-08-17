package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence.Step.{Smart, Science, Gcal}
import edu.gemini.spModel.sequence._
import jsky.app.ot.util.OtColor

import java.awt.Color

import scala.swing.event.{ValueChanged, KeyTyped, ActionEvent, KeyPressed, SelectionEvent}
import scala.swing.{CheckBox, ComboBox, Label, Reactor, TextField}

import scalaz._
import Scalaz._

/** A section of properties for some describeable A (e.g., an instrument,
  * telescope, gcal unit.
  *
  * @param aLens a partial lens from a Step[I] to A; partial because not all
  *              selected steps necessarily have an A
  * @param sel selected indices
  * @param update a function to be called when a parameter of A is changed
  * @tparam A type of the thing whose properties are to be edited
  * @tparam I type of the instrument component in use in this sequence
  */
class PropSection[A: Describe, I: Describe](
   aLens: Step[I] @?> A,
   sel: List[Step[I]],
   update: (Step[I] => Option[Step[I]]) => Unit) {

  val props = implicitly[Describe[A]].props.sortBy(_.meta.attrs.label)

  val editors = props.map { p =>
    val sLabel = new Label(p.meta.attrs.label.name)
    val lens   = aLens >=> p.lens.partial

    // Get the value if it is shared across all steps.  None otherwise.
    val initValue = sel.flatMap(lens.get) match {
      case pb :: Nil => Some(pb)
      case _         => None
    }

    // Set the value of all steps that support the property.
    def set(pb: p.B): Unit = update(lens.set(_, pb))

    p.meta match {
      case EnumMetadata(attrs, values)              =>
        new PropEditor with Reactor {
          val label     = sLabel
          val combo     = new ComboBox[p.B](values.list)
          val component = combo
          val units     = None

          reactions += {
            case _: SelectionEvent => set(combo.selection.item)
          }

          initValue.fold(combo.selection.index = -1) { pb =>
            combo.selection.item = pb
          }
          listenTo(combo.selection)
        }

      case BooleanMetadata(attrs, t, f, show, read) =>
        new PropEditor with Reactor {
          val label     = sLabel
          val check     = new CheckBox()
          val component = check
          val units     = None

          reactions += {
            case _: ActionEvent => set(if (check.selected) t else f)
          }

          initValue.fold(check.selected = false) { pb =>
            check.selected = pb == t
          }

          listenTo(check)
        }

      case TextMetadata(attrs, us, show, read)   =>
        new PropEditor with Reactor {
          val label     = sLabel
          val field     = new TextField()
          val component = field
          val units     = us.map(new Label(_))

          reactions += {
            case _: ValueChanged =>
              read(field.text) match {
                case -\/(msg) => field.background = OtColor.LIGHT_SALMON
                case \/-(pb)  => field.background = Color.WHITE
                                 set(pb)
              }
          }

          initValue.fold(field.text = "") { pb => field.text = show(pb) }
          listenTo(field)
        }
    }
  }
}

object PropSection {

  /** Extracts all relevant property sections for the selected indices of the
    * given sequence.
    *
    * @param seq the sequence to edit
    * @param sel indices of selected steps
    * @param updateSequence a function that will be called when the sequence has
    *                       been edited
    * @tparam I type of the instrument in use in the sequence
    * @return property sections used to edit the selected steps of the sequence
    */
  def sections[I: Describe](seq: Sequence[I], sel: Set[Int], updateSequence: Sequence[I] => Unit): NonEmptyList[PropSection[_, I]] = {
    val selSteps = seq.toSteps.list.zipWithIndex.collect {
      case (step, i) if sel(i) => step
    }

    def updateSteps(f: Step[I] => Option[Step[I]]): Unit = {
      val newSteps = seq.toSteps.list.zipWithIndex.map { case (step, i) =>
        if (sel(i)) f(step).fold(step)(identity) else step
      }
      updateSequence(Sequence.fromSteps(newSteps.toNel.get))
    }

    val types = (Set.empty[Step.Type]/:selSteps) { _ + _.stepType }
    val secs  = (List.empty[PropSection[_, I]]/:types) { (secs, t) =>
      t match {
        case Gcal    => new PropSection[GcalUnit, I](Step.gcal, selSteps, updateSteps)       :: secs
        case Science => new PropSection[Telescope, I](Step.telescope, selSteps, updateSteps) :: secs
        case Smart   => new PropSection[SmartCal, I](Step.smartCal, selSteps, updateSteps)   :: secs
      }
    }

    NonEmptyList.nel(new PropSection[I, I](Step.instrument, selSteps, updateSteps), secs)
  }
}
