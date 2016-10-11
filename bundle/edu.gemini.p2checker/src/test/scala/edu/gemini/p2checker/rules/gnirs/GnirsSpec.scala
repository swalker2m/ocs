package edu.gemini.p2checker.rules.gnirs

import edu.gemini.p2checker.rules.RuleSpec
import edu.gemini.pot.sp.ISPObservation
import edu.gemini.pot.sp.SPComponentType.{INSTRUMENT_GNIRS, ITERATOR_GNIRS}
import edu.gemini.spModel.gemini.gnirs.InstGNIRS.{ACQUISITION_MIRROR_PROP, CENTRAL_WAVELENGTH_PROP, FILTER_PROP}
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.AcquisitionMirror.{IN, OUT}
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.Filter._
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.Wavelength
import edu.gemini.spModel.gemini.gnirs.{SeqConfigGNIRS, InstGNIRS}
import org.specs2.matcher.MatchResult

import java.beans.PropertyDescriptor

object GnirsSpec extends RuleSpec {

  val ruleSet = new GnirsRule

  val Id = GnirsRule.PREFIX + GnirsRule.AcquisitionWavelengthRule.NAME

  def seq(init: (PropertyDescriptor, List[Any])*): ISPObservation =
    setupWithSequence[InstGNIRS, SeqConfigGNIRS](INSTRUMENT_GNIRS, ITERATOR_GNIRS)(init: _*)

  def wl(s: String): Wavelength =
    new Wavelength(s)

  def notError(init: (PropertyDescriptor, List[Any])*): MatchResult[List[String]] =
    expectNoneOf(Id) { seq(init: _*) }

  def isError(init: (PropertyDescriptor, List[Any])*): MatchResult[List[String]] =
    expectAllOf(Id) { seq(init: _*) }

  "Acquisition step central wavelength / filter rule" should {
    "not flag an error if there is no filter" in {
      notError(
        ACQUISITION_MIRROR_PROP -> List(IN),
        CENTRAL_WAVELENGTH_PROP -> List(wl("7.89"))
      )
    }

    "not flag an error if the mirror is out" in {
      notError(
        ACQUISITION_MIRROR_PROP -> List(OUT),
        CENTRAL_WAVELENGTH_PROP -> List(wl("7.89")),
        FILTER_PROP             -> List(ORDER_3)
      )
    }

    "not flag an error if the filter and wavelength matches" in {
      notError(
        ACQUISITION_MIRROR_PROP -> List(IN, IN, IN),
        CENTRAL_WAVELENGTH_PROP -> List(wl("2.2"), wl("1.65"), wl("1.25")),
        FILTER_PROP             -> List(ORDER_3, ORDER_4, ORDER_5)
      )
    }

    "flag an error if the filter and wavelength differ" in {
      isError(
        ACQUISITION_MIRROR_PROP -> List(IN, IN, IN),
        CENTRAL_WAVELENGTH_PROP -> List(wl("2.2"), wl("9.99"), wl("1.25")),
        FILTER_PROP             -> List(ORDER_3, ORDER_4, ORDER_5)
      )
    }

    "not flag an error if the filter and wavelength are close enough to round to same within 2 positions" in {
      notError(
        ACQUISITION_MIRROR_PROP -> List(IN, IN, IN),
        CENTRAL_WAVELENGTH_PROP -> List(wl("2.199"), wl("1.651"), wl("1.249")),
        FILTER_PROP             -> List(ORDER_3, ORDER_4, ORDER_5)
      )
    }
  }
}
