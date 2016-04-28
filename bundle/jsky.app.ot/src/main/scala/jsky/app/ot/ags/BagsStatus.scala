package jsky.app.ot.ags

sealed trait BagsStatus

object BagsStatus {
  case object Idle extends BagsStatus
  case object Pending extends BagsStatus
  case object Running extends BagsStatus
  case class Failed(why: String) extends BagsStatus
  case object Error extends BagsStatus
}
