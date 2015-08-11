package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence.{TextMetadata, BooleanMetadata, EnumMetadata, Describe, Prop, Step}

import scala.swing.{ComboBox, Component, Label}

import scalaz._
import Scalaz._

sealed trait PropEditor[I] {
  def prop: Prop[Step[I]]
  def label: Label
  def components: NonEmptyList[Component]

  def edit(steps: List[Step[I]]): Unit
}

object PropEditor {

  def editorFor[I: Describe](p: Prop[Step[I]]): PropEditor[I] =
    p.meta match {

      case EnumMetadata(attrs, values)              =>
        new PropEditor[I] {
          val prop  = p
          val label = new Label(attrs.label.name)
          val combo = new ComboBox[p.B](values.list)
          val components = combo.wrapNel

          def edit(steps: List[Step[I]]): Unit = {

          }
        }

      case BooleanMetadata(attrs, t, f, _, _)       => ???

      case TextMetadata(attrs, units, show, read)   => ???
    }
}