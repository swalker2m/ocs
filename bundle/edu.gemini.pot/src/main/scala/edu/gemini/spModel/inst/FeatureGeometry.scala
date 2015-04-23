package edu.gemini.spModel.inst

import java.awt.Shape
import java.awt.geom.{Point2D, AffineTransform}

import edu.gemini.shared.util.immutable.{DefaultImList, ImList}
import edu.gemini.spModel.core.{OffsetP, OffsetQ, Offset, Angle}
import edu.gemini.spModel.core.AngleSyntax._
import edu.gemini.spModel.inst.ProbeArmGeometry.ArmAdjustment
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.rich.shared.immutable._

import scala.collection.JavaConverters._
import scalaz._
import Scalaz._

/**
 * Functions to manipulate the geometry for a telescope feature, e.g. a science
 * area or a guide probe arm.
 */
object FeatureGeometry {

  def offsetTransform(posAngle: Angle, offset: Offset): AffineTransform =
    posAngleTransform(posAngle) <| {
      _.translate(-offset.p.arcsecs, -offset.q.arcsecs)
    }

  def posAngleTransform(posAngle: Angle): AffineTransform =
    AffineTransform.getRotateInstance(-posAngle.toRadians)

  /** Transform to screen coordinates.
   * @param base   base position screen coordinates
   * @param scale  screen scale or pixels per arcsec
   * @param rot    rotation of the image from north
   * @param flipRa true if RA increases going right
   * @return transform that will map to screen coordinates
   */
  def screenTransform(base: Point2D, scale: Double, rot: Angle, flipRa: Boolean): AffineTransform =
    AffineTransform.getTranslateInstance(base.getX, base.getY) <|
      (_.scale(scale, scale))                                  <|
      (_.rotate(-rot.toRadians))                               <|
      (_.scale(if (flipRa) -1 else 1, 1))

  implicit class PointOps(p: Point2D) {
    def toOffset: Offset =
      Offset(p.getX.arcsecs[OffsetP], p.getY.arcsecs[OffsetQ]) * -1
  }

  implicit class OffsetOps(o: Offset) {
    def toPoint: Point2D =
      new Point2D.Double(-o.p.arcsecs, -o.q.arcsecs)

    def rotate(posAngle: Angle): Offset = {
      val result = new Point2D.Double
      posAngleTransform(posAngle).transform(o.toPoint, result)
      result.toOffset
    }
  }

  // ------------------------------------------------------------------------


  /**
   * Convenience method to execute a transformation on a point and return the result.
   * @param p     the point to transform
   * @param trans the transformation to execute
   * @return      the transformed point
   */
  def transformPoint(p: Point2D, trans: AffineTransform): Point2D = {
    val pTrans = new Point2D.Double()
    trans.transform(p, pTrans)
    pTrans
  }

  /**
   * Given a list of shapes representing a science area, transform them as needed for the specified context.
   * @param shapes the list of shapes to transform
   * @param ctx    the context under which to transform them
   * @return       the transformed shapes
   */
  def transformScienceAreaForContext(shapes: List[Shape], ctx: ObsContext): List[Shape] = {
    import edu.gemini.pot.ModelConverters._
    val posAngle = ctx.getPositionAngle.toRadians.getMagnitude
    val basePoint = {
      val coords = ctx.getBaseCoordinates.toNewModel
      new Point2D.Double(coords.ra.toAngle.toNormalizedArcseconds, coords.dec.toAngle.toNormalizedArcseconds)
    }
    val trans = AffineTransform.getRotateInstance(-posAngle, basePoint.getX, basePoint.getY)
    trans.translate(basePoint.getX, basePoint.getY)
    shapes.map{ trans.createTransformedShape }
  }

  def transformScienceAreaForContextAsJava(shapes: List[Shape], ctx: ObsContext): ImList[Shape] =
    DefaultImList.create(transformScienceAreaForContext(shapes, ctx).asJava)
  def transformScienceAreaForContextAsJava(shapes: ImList[Shape], ctx: ObsContext): ImList[Shape] =
    transformScienceAreaForContextAsJava(shapes.asScalaList, ctx)

  /**
   * Given a single shape representing part or all of a science area, transform it as needed for the context.
   * @param shape the shape to transform
   * @param ctx   the context under which to transform it
   * @return      the transformed shape
   */
  def transformScienceAreaForContext(shape: Shape, ctx: ObsContext): Shape =
    transformScienceAreaForContext(List(shape), ctx).head

  /**
   * Given a list of shapes representing a science area, transform it to screen coordinates.
   * @param shapes          the list of shapes to transform
   * @param pixelsPerArcsec the pixel density
   * @return                the shapes scaled for the display
   */
  def transformScienceAreaForScreen(shapes: List[Shape], pixelsPerArcsec: Double, ctx: ObsContext, screenPos: Point2D): List[Shape] = {
    import edu.gemini.pot.ModelConverters._
    val trans = AffineTransform.getScaleInstance(pixelsPerArcsec, pixelsPerArcsec)
    val x = screenPos.getX / pixelsPerArcsec - ctx.getBaseCoordinates.toNewModel.ra.toAngle.toNormalizedArcseconds
    val y = screenPos.getY / pixelsPerArcsec - ctx.getBaseCoordinates.toNewModel.dec.toAngle.toNormalizedArcseconds
    trans.translate(x, y)
    shapes.map { trans.createTransformedShape }
  }

  /**
   * Given a shape representing part or all of a science area, transform it to screen coordinates.
   * @param shape          the shape to transform
   * @param pixelsPerArcsec the pixel density
   * @return                the shape scaled for the display
   */
  def transformScienceAreaForScreen(shape: Shape, pixelsPerArcsec: Double, ctx: ObsContext, screenPos: Point2D): Shape =
    transformScienceAreaForScreen(List(shape), pixelsPerArcsec, ctx, screenPos).head

  /**
   * Given a list of shapes representing a guide probe arm, an angle for the probe arm, and the position of the guide
   * star, execute transformations on the shapes to get the adjusted probe arm.
   * @param shapes         the list of shapes to transform
   * @param armAdjustment  the adjustment information for the probe arm, consisting of an angle and guide star location
   * @return               the transformed shapes
   */
  def transformProbeArmForContext(shapes: List[Shape], armAdjustment: ArmAdjustment): List[Shape] = {
    // For the guide star, we want to use the point closest to zero in terms of arcsec as a normalization.
//    val armAngle  = armAdjustment.angle
//    val guideStar = armAdjustment.guideStar
//    val armTrans  = AffineTransform.getRotateInstance(armAngle.toRadians, guideStar.getX, guideStar.getY)
//    armTrans.concatenate(AffineTransform.getTranslateInstance(guideStar.getX, guideStar.getY))
//    shapes.map { armTrans.createTransformedShape }
    shapes
  }

  /**
   * Given a shape representing a guide probe arm or segment, an angle for the probe arm, and the position of the guide
   * star, execute transformations on the shape to get the adjusted probe arm.
   * @param shape          the shape to transform
   * @param armAdjustment  the adjustment information for the probe arm, consisting of an angle and guide star location
   * @return               the transformed shapes
   */
  def transformProbeArmForContext(shape: Shape, armAdjustment: ArmAdjustment): Shape =
    transformProbeArmForContext(List(shape), armAdjustment).head

  /**
   * Given a list of geometry shapes, transform them as needed for display on the screen.
   * @param shapes          the list of shapes to transform
   * @param pixelsPerArcsec the pixel density per arcsec on the current display
   * @param flipRA          a scaling in the y-axis: should be either 1.0 or -1.0, and not sure if this is ever -1.0
   * @param screenPos       the final position on the screen where the adjusted guide probe arm should be placed
   * @return                the transformed shape
   */
  def transformProbeArmForScreen(shapes: List[Shape], pixelsPerArcsec: Double, flipRA: Double, screenPos: Point2D): List[Shape] = {
    val trans = AffineTransform.getTranslateInstance(screenPos.getX, screenPos.getY)
    trans.scale(pixelsPerArcsec, pixelsPerArcsec)
    shapes.map { trans.createTransformedShape }
  }

  /**
   * Given a shape, transform it as needed for display on the screen.
   * @param shape           the shape to transform
   * @param pixelsPerArcsec the pixel density per arcsec on the current display
   * @param flipRA          a scaling in the y-axis: should be either 1.0 or -1.0, and not sure if this is ever -1.0
   * @param screenPos       the final position on the screen where the adjusted guide probe arm should be placed
   * @return                the transformed shape
   */
  def transformProbeArmForScreen(shape: Shape, pixelsPerArcsec: Double, flipRA: Double, screenPos: Point2D): Shape =
    transformProbeArmForScreen(List(shape), pixelsPerArcsec, flipRA, screenPos).head
}

