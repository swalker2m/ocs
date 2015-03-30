package edu.gemini.ags.api

import edu.gemini.ags.api.AgsGuideQuality.Unusable
import edu.gemini.ags.api.AgsMagnitude._
import edu.gemini.ags.impl._
import edu.gemini.spModel.core.Target.SiderealTarget
import edu.gemini.spModel.core.{Magnitude, MagnitudeBand}
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.ImageQuality
import edu.gemini.spModel.guide.{ValidatableGuideProbe, GuideProbe, GuideProbeGroup, GuideSpeed}
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.rich.shared.immutable._
import edu.gemini.spModel.target.SPTarget

sealed trait AgsGuideQuality {
  def message: String
}

object AgsGuideQuality {
  case object DeliversRequestedIq extends AgsGuideQuality {
    override val message = "Delivers requested IQ."
  }
  case object PossibleIqDegradation extends AgsGuideQuality {
    override val message = "Slower guiding required; may not deliver requested IQ."
  }
  case object IqDegradation extends AgsGuideQuality {
    override val message = "Slower guiding required; will not deliver requested IQ."
  }
  case object PossiblyUnusable extends AgsGuideQuality {
    override val message = "May not be able to guide."
  }
  case object Unusable extends AgsGuideQuality {
    override val message = "Unable to guide."
  }

  val All: List[AgsGuideQuality] =
    List(DeliversRequestedIq, PossibleIqDegradation, IqDegradation, PossiblyUnusable, Unusable)
}

sealed trait AgsAnalysis {
  def quality: AgsGuideQuality = Unusable
  def probeBands: List[MagnitudeBand]
  def message(withProbe: Boolean): String
}

object AgsAnalysis {
  case class NoGuideStarForProbe(guideProbe: GuideProbe, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"${guideProbe.getKey} " else ""
      s"No ${p}guide star selected."
    }
  }

  case class NoGuideStarForGroup(guideGroup: GuideProbeGroup, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String =
      s"No ${guideGroup.getKey} guide star selected."
  }

  case class MagnitudeTooFaint(guideProbe: GuideProbe, target: SiderealTarget, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"use ${guideProbe.getKey}" else "guide"
      s"Cannot $p with the star in these conditions, even using the slowest guide speed."
    }
  }

  case class MagnitudeTooBright(guideProbe: GuideProbe, target: SiderealTarget, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"${guideProbe.getKey} g" else "G"
      s"${p}uide star is too bright to guide."
    }
  }

  case class NotReachable(guideProbe: GuideProbe, target: SiderealTarget, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"with ${guideProbe.getKey} " else ""
      s"The star is not reachable ${p}at all positions."
    }
  }

  case class NoMagnitudeForBand(guideProbe: GuideProbe, target: SiderealTarget, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"${guideProbe.getKey} g" else "G"
      if (probeBands.length == 1) {
        s"${p}uide star ${probeBands.head}-band magnitude is missing. Cannot determine guiding performance."
      } else {
        s"${p}uide star ${probeBands.map(_.name).mkString(", ")}-band magnitudes are missing. Cannot determine guiding performance."
      }
    }
    override val quality = AgsGuideQuality.PossiblyUnusable
  }

  case class Usable(guideProbe: GuideProbe, target: SiderealTarget, guideSpeed: GuideSpeed, override val quality: AgsGuideQuality, probeBands: List[MagnitudeBand]) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val qualityMessage = quality match {
        case AgsGuideQuality.DeliversRequestedIq => ""
        case _                                   => s"${quality.message} "
      }
      val p = if (withProbe) s"${guideProbe.getKey} " else ""

      s"$qualityMessage${p}Guide Speed: ${guideSpeed.name}."
    }
  }

  def guideProbe(a: AgsAnalysis): Option[GuideProbe] = a match {
    case NoGuideStarForProbe(p, _)   => Some(p)
    case NoGuideStarForGroup(_, _)   => None
    case MagnitudeTooFaint(p, _, _)  => Some(p)
    case MagnitudeTooBright(p, _, _) => Some(p)
    case NotReachable(p, _, _)       => Some(p)
    case NoMagnitudeForBand(p, _, _) => Some(p)
    case Usable(p, _, _, _, _)       => Some(p)
  }


  /**
   * Analysis of the selected guide star (if any) in the given context.
   */
  protected [ags] def analysis(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, bands: List[MagnitudeBand]): Option[AgsAnalysis] = {
    def selection(ctx: ObsContext, guideProbe: GuideProbe): Option[SPTarget] =
      for {
        gpt   <- ctx.getTargets.getPrimaryGuideProbeTargets(guideProbe).asScalaOpt
        gStar <- gpt.getPrimary.asScalaOpt
      } yield gStar

    selection(ctx, guideProbe).fold(Some(NoGuideStarForProbe(guideProbe, bands)): Option[AgsAnalysis]) { guideStar =>
      AgsAnalysis.analysis(ctx, mt, guideProbe, guideStar, bands)
    }
  }

  /**
   * Analysis of the given guide star in the given context, regardless of which
   * guide star is actually selected in the target environment.
   */
  protected [ags] def analysis(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, guideStar: SPTarget, bands: List[MagnitudeBand]): Option[AgsAnalysis] =
    if (!guideProbe.validate(guideStar, ctx)) Some(NotReachable(guideProbe, guideStar.toNewModel, bands))
    else magnitudeAnalysis(ctx, mt, guideProbe, guideStar.toNewModel, bands)

  private def magnitudeAnalysis(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, guideStar: SiderealTarget, bands: List[MagnitudeBand]): Option[AgsAnalysis] = {
    import GuideSpeed._
    import AgsGuideQuality._

    val conds = ctx.getConditions

    // Handles the case where the magnitude falls outside of the acceptable ranges for any guide speed.
    // This handles Andy's 0.5 rule where we might possibly be able to guide if the star is only 0.5 too dim, and
    // otherwise returns the appropriate analysis indicating too dim or too bright.
    def outsideLimits(magCalc: MagnitudeCalc, mag: Double): AgsAnalysis = {
      val adj             = 0.5
      val saturationLimit = magCalc(conds, FAST).saturationConstraint
      val faintnessLimit  = magCalc(conds, SLOW).faintnessConstraint.brightness
      val saturated       = saturationLimit.exists(_.brightness > mag)

      def almostTooFaint: Boolean = !saturated && mag <= faintnessLimit + adj
      def tooFaint:       Boolean = mag > faintnessLimit + adj

      if (almostTooFaint) Usable(guideProbe, guideStar, SLOW, PossiblyUnusable, bands)
      else if (tooFaint)  MagnitudeTooFaint(guideProbe, guideStar, bands)
      else                MagnitudeTooBright(guideProbe, guideStar, bands)
    }

    // Called when we know that a valid guide speed can be chosen for the given guide star.
    // Determine the quality and return an analysis indicating that the star is usable.
    def usable(guideSpeed: GuideSpeed): AgsAnalysis = {
      def worseOrEqual(iq: ImageQuality) = conds.iq.compareTo(iq) >= 0

      val quality = guideSpeed match {
        case FAST =>
          DeliversRequestedIq
        case MEDIUM =>
          if (worseOrEqual(ImageQuality.PERCENT_70)) DeliversRequestedIq
          else PossibleIqDegradation
        case SLOW =>
          if (worseOrEqual(ImageQuality.PERCENT_85)) DeliversRequestedIq
          else if (worseOrEqual(ImageQuality.PERCENT_70)) PossibleIqDegradation
          else IqDegradation
      }

      Usable(guideProbe, guideStar, guideSpeed, quality, bands)
    }

    // Find the first band in the guide star that is on the list of possible bands
    def usableMagnitude:Option[Magnitude] = bands.map(guideStar.magnitudeIn).find(_.isDefined).flatten

    for {
      mc  <- mt(ctx, guideProbe)
      mag = usableMagnitude
    } yield {
      val analysisOpt = mag.map(m => fastestGuideSpeed(mc, m.value, conds).fold(outsideLimits(mc, m.value))(usable))
      analysisOpt.getOrElse(NoMagnitudeForBand(guideProbe, guideStar, bands))
    }
  }
}