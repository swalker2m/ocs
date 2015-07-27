package edu.gemini.spModel.sequence

import scalaz.NonEmptyList

sealed trait Metadata {
  def attrs: Metadata.Attributes
}

object Metadata {
  sealed trait Scope
  object Scope {
    case object Global     extends Scope
    case object SingleStep extends Scope
  }

  sealed trait Access
  object Access {
    case object Science     extends Access
    case object Engineering extends Access
  }

  case class Attributes(name: String, scope: Scope, access: Access, description: String = "")
}

import Metadata.Attributes

case class EnumMetadata[A](attrs: Attributes, values: NonEmptyList[A]) extends Metadata
case class ValueMetadata[A](attrs: Attributes, default: A) extends Metadata
