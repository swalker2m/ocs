package edu.gemini.ags.api

import edu.gemini.ags.api.AgsAnalysis.NotReachable
import edu.gemini.ags.api.AgsMagnitude.{MagnitudeCalc, MagnitudeTable}
import edu.gemini.catalog.api.CatalogQuery
import edu.gemini.pot.ModelConverters._
import edu.gemini.spModel.ags.AgsStrategyKey
import edu.gemini.spModel.core.{BandsList, Angle}
import edu.gemini.spModel.core.SiderealTarget
import edu.gemini.spModel.guide.{GuideStarValidation, ValidatableGuideProbe, GuideProbe}
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.rich.shared.immutable._
import edu.gemini.shared.util.immutable.{Option => JOption, Some => JSome}
import edu.gemini.spModel.target.SPTarget
import edu.gemini.spModel.target.env._
import edu.gemini.spModel.target.system.HmsDegTarget

import scala.concurrent.Future

import scalaz._
import Scalaz._

trait AgsStrategy {
  def key: AgsStrategyKey

  def magnitudes(ctx: ObsContext, mt: MagnitudeTable): List[(GuideProbe, MagnitudeCalc)]

  def analyze(ctx: ObsContext, mt: MagnitudeTable): List[AgsAnalysis]

  def analyze(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, guideStar: SiderealTarget): Option[AgsAnalysis]

  def analyzeForJava(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, guideStar: SiderealTarget): JOption[AgsAnalysis] = {
    val spTarget = new SPTarget(guideStar.coordinates.ra.toAngle.toDegrees, guideStar.coordinates.dec.toDegrees)
    if (guideProbe.validate(spTarget, ctx) != GuideStarValidation.VALID) new JSome(NotReachable(guideProbe, guideStar, probeBands))
    else analyze(ctx, mt, guideProbe, guideStar).asGeminiOpt
  }

  def candidates(ctx: ObsContext, mt: MagnitudeTable): Future[List[(GuideProbe, List[SiderealTarget])]]

  /**
   * Returns a list of catalog queries that would be used to search for guide stars with the given context
   */
  def catalogQueries(ctx: ObsContext, mt: MagnitudeTable): List[CatalogQuery]

  def estimate(ctx: ObsContext, mt: MagnitudeTable): Future[AgsStrategy.Estimate]

  def select(ctx: ObsContext, mt: MagnitudeTable): Future[Option[AgsStrategy.Selection]]

  def guideProbes: List[GuideProbe]

  /**
   * Indicates the bands that will be used for a given probe
   */
  def probeBands: BandsList
}

object AgsStrategy {
  object Estimate {
    val CompleteFailure   = Estimate(0.0)
    val GuaranteedSuccess = Estimate(1.0)

    def toEstimate(probability: Double): Estimate =
      Estimate(probability).normalize
  }

  /**
   * Estimation of success of finding a guide star at phase 2 time.
   */
  case class Estimate(probability: Double) extends AnyVal {
    def normalize: Estimate =
      if (probability <= 0) Estimate.CompleteFailure
      else if (probability >= 1) Estimate.GuaranteedSuccess
      else this
  }

  /**
   * An assignment of a guide star to a particular guide probe.
   */
  case class Assignment(guideProbe: GuideProbe, guideStar: SiderealTarget)

  /**
   * Results of running an AGS selection.  The position angle for which the
   * results are valid along with all assignments of guide probes to stars.
   */
  case class Selection(posAngle: Angle, assignments: List[Assignment]) {
    /**
     * Creates a new TargetEnvironment with guide stars for each assignment in
     * the Selection.
     */
    def applyTo(env: TargetEnvironment): TargetEnvironment = {
      val targetMap = ==>>.fromList(assignments.map { case Assignment(gp,gs) =>
        gp -> new SPTarget(HmsDegTarget.fromSkyObject(gs.toOldModel))})
      val newAuto: AutomaticGroup = AutomaticGroup.Active(targetMap, posAngle)

      // If this is different from the old automatic GG, then replace.
      val oldGuideEnvironment = env.getGuideEnvironment
      val oldGuideEnv         = oldGuideEnvironment.guideEnv

      // SPTargets are compared by references, so we extract the names and compare.
      def extractNames(auto: AutomaticGroup) = auto.targetMap.map(_.getName)

      if (extractNames(oldGuideEnv.auto) =/= extractNames(newAuto)) {
        val newGuideEnv = oldGuideEnv.copy(auto = newAuto)
        val newGuideEnvironment = oldGuideEnvironment.copy(guideEnv = newGuideEnv)
        env.setGuideEnvironment(newGuideEnvironment)
      } else {
        env
      }
    }
  }
}
