package edu.gemini.spModel.sequence

import scala.language.implicitConversions

trait Default[A] { self =>
  def default: A
}

object Default {
  def forValue[A](a: A): Default[A] =
    new Default[A] {
      val default = a
    }
}
