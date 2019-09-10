package edu.gemini.ags.impl

import edu.gemini.ags.api.{AgsAnalysis, AgsMagnitude, AgsStrategy, ProbeCandidates}
import edu.gemini.ags.api.AgsStrategy.{Assignment, Estimate, Selection}
import edu.gemini.ags.gems._
import edu.gemini.catalog.api._
import edu.gemini.catalog.api.CatalogName.UCAC4
import edu.gemini.catalog.votable._
import edu.gemini.pot.sp.SPComponentType
import edu.gemini.pot.ModelConverters._
import edu.gemini.spModel.core.SiderealTarget
import edu.gemini.spModel.ags.AgsStrategyKey
import edu.gemini.spModel.gemini.flamingos2.{Flamingos2, Flamingos2OiwfsGuideProbe}
import edu.gemini.spModel.gemini.gems.{CanopusWfs, GemsInstrument}
import edu.gemini.spModel.gemini.gsaoi.{Gsaoi, GsaoiOdgw}
import edu.gemini.spModel.gems.GemsGuideProbeGroup
import edu.gemini.spModel.guide.OrderGuideGroup
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.rich.shared.immutable._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import edu.gemini.ags.api.AgsMagnitude.{MagnitudeCalc, MagnitudeTable}
import edu.gemini.spModel.guide.{GuideProbe, GuideProbeGroup, ValidatableGuideProbe}
import edu.gemini.spModel.core._
import edu.gemini.spModel.telescope.PosAngleConstraint

import scalaz._
import Scalaz._

final case class GemsStrategy(
  catalogName: CatalogName,
  backend:     Option[VoTableBackend]
) extends AgsStrategy {

  import GemsStrategy._

  override def key: AgsStrategyKey =
    AgsStrategyKey.GemsKey

  // NGS2 GeMS has no concept of guide speed.
  private val showGuideSpeed: Boolean =
    false

  // Since the constraints are run in parallel, we need a way to identify them after
  // they are done, so we create IDs for each. This is a pretty nasty way to do things, but
  // since we cannot predict in what order the results will return, we need to be able
  // to pick them out somehow.
  private val CanopusTipTiltId = 0
  private val OdgwFlexureId    = 1

  override def magnitudes(ctx: ObsContext, mt: MagnitudeTable): List[(GuideProbe, MagnitudeCalc)] = {
    val cans = CanopusWfs.values().map { cwfs => mt(ctx, cwfs).map(cwfs -> _) }.toList.flatten
    val odgw = GsaoiOdgw.values().map { odgw => mt(ctx, odgw).map(odgw -> _) }.toList.flatten
    cans ++ odgw
  }

  override def analyze(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, guideStar: SiderealTarget): Option[AgsAnalysis] =
    AgsAnalysis.analysis(ctx, mt, guideProbe, guideStar, showGuideSpeed = showGuideSpeed)

  def analyzeMagnitude(ctx: ObsContext, mt: MagnitudeTable, guideProbe: ValidatableGuideProbe, guideStar: SiderealTarget): Option[AgsAnalysis] =
    AgsAnalysis.magnitudeAnalysis(ctx, mt, guideProbe, guideStar, showGuideSpeed = showGuideSpeed)

  override def analyze(ctx: ObsContext, mt: MagnitudeTable): List[AgsAnalysis] = {
    import AgsAnalysis._

    def mapGroup(grp: GuideProbeGroup): List[AgsAnalysis] = {
      def hasGuideStarForProbe(a: AgsAnalysis): Boolean = a match {
        case NoGuideStarForProbe(_) => false
        case _                      => true
      }

      val probeAnalysis = grp.getMembers.asScala.toList.flatMap { p => analysis(ctx, mt, p, showGuideSpeed = showGuideSpeed) }
      probeAnalysis.filter(hasGuideStarForProbe) match {
        case Nil =>
          // Pick the first guide probe as representative, since we are called with either Canopus or GsaoiOdwg
          ~grp.getMembers.asScala.headOption.map {gp => List(NoGuideStarForGroup(grp))}
        case lst => lst
      }
    }

    mapGroup(CanopusWfs.Group.instance) // TODO: REL-2941 ++ mapGroup(GsaoiOdgw.Group.instance)
  }

  override def candidates(ctx: ObsContext, mt: MagnitudeTable)(ec: ExecutionContext): Future[List[ProbeCandidates]] = {

    // Extract something we can understand from the GemsCatalogSearchResults.
    def simplifiedResult(results: Option[GemsCatalogSearchResults]): List[ProbeCandidates] =
      results.toList.flatMap { result =>
        val so = result.results  // extract the sky objects from this thing
        // For each guide probe associated with these sky objects, add a tuple
        // (guide probe, sky object list) to the results
        result.criterion.key.group.getMembers.asScala.toList.map { guideProbe =>
          ProbeCandidates(guideProbe, so)
        }
      }

    // Here we don't care about position angle because we aren't doing an
    // analysis.
    search(ctx, Set(Angle.zero))(ec).map(simplifiedResult)
  }

  override def estimate(ctx: ObsContext, mt: MagnitudeTable)(ec: ExecutionContext): Future[Estimate] = {
    // Create a set of the angles to try.
    val anglesToTry = (0 until 360 by 90).map(Angle.fromDegrees(_)).toSet

    // Iterate over 90 degree position angles if no 3-star asterism is found at PA = 0.
    val gemsCatalogResults: Future[List[GemsGuideStars]] =
      search(ctx, anglesToTry)(ec).map { result =>
        val targets = result.fold(List.empty[SiderealTarget])(_.results)
        GemsResultsAnalyzer.analyzeGoodEnough(ctx, anglesToTry, targets, _.stars.size < 3)
      }

    // Determine if the asterisms are big enough.
    gemsCatalogResults.map { ggsLst =>
      val largestAsterism = ggsLst.map(_.guideGroup.grp.toManualGroup.targetMap.keySet.intersection(GemsStrategy.canopusProbes).size).fold(0)(math.max)
      AgsStrategy.Estimate.toEstimate(largestAsterism / 3.0)
    }
  }

  protected [impl] def search(
    ctx:       ObsContext,
    posAngles: Set[Angle]
  )(
    ec: ExecutionContext
  ): Future[Option[GemsCatalogSearchResults]] =

    // TODO-NGS: we need to search from the center of the bounding box of the
    // intersection of the AO areas at all offset positions, not the base
    // position itself.
    ctx.getBaseCoordinates.asScalaOpt.fold(Future.successful(Option.empty[GemsCatalogSearchResults])) { base =>
      // Get the instrument: F2 or GSAOI?
      val gemsInstrument =
        (ctx.getInstrument.getType == SPComponentType.INSTRUMENT_GSAOI) ? GemsInstrument.gsaoi | GemsInstrument.flamingos2

      // Search options
      val gemsOptions = new GemsGuideStarSearchOptions(gemsInstrument, posAngles.asJava)

      // Perform the catalog search, using GemsStrategy's backend
      GemsVoTableCatalog(catalogName, backend)
        .search(ctx, base.toNewModel, gemsOptions)(ec)
        .map(r => Some(r))
    }

  override def select(ctx: ObsContext, mt: MagnitudeTable)(ec: ExecutionContext): Future[Option[Selection]] = {
    val posAngles = ctx.getInstrument.getType match {
      case SPComponentType.INSTRUMENT_GSAOI if ctx.getInstrument.asInstanceOf[Gsaoi].getPosAngleConstraint == PosAngleConstraint.FIXED =>
        Set(ctx.getPositionAngle)
      case SPComponentType.INSTRUMENT_FLAMINGOS2 if ctx.getInstrument.asInstanceOf[Flamingos2].getPosAngleConstraint == PosAngleConstraint.FIXED =>
        Set(ctx.getPositionAngle)
      case _ =>
        Set(ctx.getPositionAngle, Angle.zero, Angle.fromDegrees(90), Angle.fromDegrees(180), Angle.fromDegrees(270))
    }

    search(ctx, posAngles)(ec).map { r =>
      val gemsGuideStars =
        GemsResultsAnalyzer
          .analyze(ctx, posAngles, r.toList.flatMap(_.results), None)
          .headOption // they are ordered by strehl and we want the best strehl

      // Now we must convert from an Option[GemsGuideStars] to a Selection.
      gemsGuideStars.map { x =>
        val assignments = x.guideGroup.getAll.asScalaList.filter(_.getGuider.getGroup.contains(CanopusWfs.Group.instance)).flatMap(targets => {
          val guider = targets.getGuider
          targets.getTargets.asScalaList.map(target => Assignment(guider, target.toSiderealTarget(ctx.getSchedulingBlockStart)))
        })
        Selection(x.pa, assignments)
      }
    }
  }

  override def catalogQueries(ctx: ObsContext, mt: MagnitudeTable): List[CatalogQuery] =
    ctx.getBaseCoordinates.asScalaOpt.fold(List.empty[CatalogQuery]) { base =>
      import AgsMagnitude._
      val cond = ctx.getConditions
      val mags = magnitudes(ctx, mt).toMap

      def lim(gp: GuideProbe): Option[MagnitudeConstraints] = autoSearchConstraints(mags(gp), cond)

      val odgwMagLimits = (lim(GsaoiOdgw.odgw1) /: GsaoiOdgw.values().drop(1)) { (ml, odgw) =>
        (ml |@| lim(odgw))(_ union _).flatten
      }
      val canMagLimits = (lim(CanopusWfs.cwfs1) /: CanopusWfs.values().drop(1)) { (ml, can) =>
        (ml |@| lim(can))(_ union _).flatten
      }

      val canopusConstraint = canMagLimits.map(c => CatalogQuery(CanopusTipTiltId, base.toNewModel, RadiusConstraint.between(Angle.zero, CanopusWfs.Group.instance.getRadiusLimits.toNewModel), List(ctx.getConditions.adjust(c)), catalogName))
      val odgwConstraint    = odgwMagLimits.map(c => CatalogQuery(OdgwFlexureId,   base.toNewModel, RadiusConstraint.between(Angle.zero, GsaoiOdgw.Group.instance.getRadiusLimits.toNewModel), List(ctx.getConditions.adjust(c)), catalogName))
      List(canopusConstraint, odgwConstraint).flatten
    }

  override val probeBands = RBandsList

  // Return the band used for each probe
  // TODO Delegate to GemsMagnitudeTable
  private def probeBands(guideProbe: GuideProbe): BandsList = if (CanopusWfs.Group.instance.getMembers.contains(guideProbe)) RBandsList else SingleBand(MagnitudeBand.H)

  override val guideProbes: List[GuideProbe] =
    Flamingos2OiwfsGuideProbe.instance :: (GsaoiOdgw.values() ++ CanopusWfs.values()).toList
}

object GemsStrategy {

  private val canopusProbes: ISet[GuideProbe] =
    ISet.fromList(List(CanopusWfs.cwfs1, CanopusWfs.cwfs2, CanopusWfs.cwfs3))

}
