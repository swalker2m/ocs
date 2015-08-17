package jsky.app.ot.editor.sequence

import scala.swing.{Component, Label}

trait PropEditor {
  def label: Label
  def component: Component
  def units: Option[Label]
}