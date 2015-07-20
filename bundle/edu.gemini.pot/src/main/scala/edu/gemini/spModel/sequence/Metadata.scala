package edu.gemini.spModel.sequence

import scalaz.NonEmptyList

/**
 *
 */
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

  sealed trait Descriptor {
    def attrs: Attributes
  }

  case class EnumDescriptor[A](attrs: Attributes, values: NonEmptyList[A]) extends Descriptor
  case class RangeDescriptor[A <: Ordered[A]](attrs: Attributes, min: A, max: A) extends Descriptor
  case class ValueDescriptor[A](attrs: Attributes, default: A) extends Descriptor
}
