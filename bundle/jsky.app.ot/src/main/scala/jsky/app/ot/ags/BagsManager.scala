package jsky.app.ot.ags

import jsky.app.ot.ags.BagsState.{IdleState, ErrorTransition, StateTransition}

import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.time.Instant
import java.util.concurrent.TimeoutException
import java.util.concurrent._
import java.util.logging.Logger

import edu.gemini.ags.api.{AgsHash, AgsRegistrar, AgsStrategy}
import edu.gemini.catalog.votable.{CatalogException, GenericError}
import edu.gemini.pot.sp._
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.spModel.guide.GuideProbe
import edu.gemini.spModel.obs.{ObsClassService, ObservationStatus, SPObservation}
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.obsclass.ObsClass
import edu.gemini.spModel.rich.shared.immutable._
import edu.gemini.spModel.target.env.{AutomaticGroup, GuideEnv, GuideEnvironment}
import jsky.app.ot.OT
import jsky.app.ot.gemini.altair.Altair_WFS_Feature
import jsky.app.ot.gemini.inst.OIWFS_Feature
import jsky.app.ot.gemini.tpe.TpePWFSFeature
import jsky.app.ot.tpe.{TpeContext, TpeManager}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.swing.Swing
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO.ioUnit

/** The state of AGS lookup for a single observation. */
sealed trait BagsState {
  /** Gets the corresponding status for the UI.*/
  def status: BagsStatus

  /** Called when an observation is edited to determine what should be the new
    * state.  This is typically called in response to a listener on the
    * science program.
    */
  def edit: StateTransition =
    ErrorTransition

  /** Called when a timer goes off to evaluate what should be done next and
    * possibly move to a new state.  Timer events transition out of pending
    * into running or out of failed into pending.  When an AGS lookup fails,
    * a few seconds are added before transitioning in order to provide an
    * opportunity for network issues, etc. to sort themselves out.
    */
  def wakeUp: StateTransition =
    (this, ioUnit)

  /** Called with the successful results of an AGS lookup. */
  def succeed(results: Option[AgsStrategy.Selection]): StateTransition =
    ErrorTransition

  /** Called to notify that an AGS lookup failed. */
  def fail(why: String, delayMs: Long): StateTransition =
    ErrorTransition
}


object BagsState {
  type AgsHashVal      = Long
  type StateTransition = (BagsState, IO[Unit]) // Next BagsState, side-effects
  val ErrorTransition  = (ErrorState, ioUnit)  // Default transition for unexpected events

  /** Error.  This state represents a logic error.  There are no transitions out
    * of the error state.
    */
  case object ErrorState extends BagsState {
    override val status: BagsStatus = BagsStatus.Error
  }

  /** Idle.  When idle, there are no pending or running AGS lookups.  This is
    * where we hang out waiting for something to be edited in order to kick off
    * another AGS search.
    */
  case class IdleState(obs: ISPObservation, hash: Option[AgsHashVal]) extends BagsState {
    override val status: BagsStatus = BagsStatus.Idle

    override val edit: StateTransition =
      (PendingState(obs, hash), BagsManager.wakeUpAction(obs, 0))
  }

  /** Pending.  When pending, we assume we've been edited and that a new AGS
    * search may be required.  This is a transient state since we're expecting
    * the timer to go off, wake us up and then we'll decide whether to do an
    * AGS lookup, update the AGS hash code, etc.
    */
  case class PendingState(obs: ISPObservation, hash: Option[AgsHashVal]) extends BagsState {
    override val status: BagsStatus = BagsStatus.Pending

    override val edit: StateTransition =
      (this, ioUnit)

    override def wakeUp: StateTransition = {
      def notObserved: Boolean =
        ObservationStatus.computeFor(obs) != ObservationStatus.OBSERVED

      // Get the ObsContext and AgsStrategy, if possible.  If not possible,
      // we won't be able to do AGS for this observation.
      val tup = for {
        c <- ObsContext.create(obs).asScalaOpt
        s <- agsStrategy(obs, c)
      } yield (c, s)

      // Compute the new hash value of the observation, if possible.  It may
      // be possible to (re)run AGS but not necessary if the hash hasn't
      // changed.
      val newHash = tup.map { case (ctx, _) => hashObs(obs, ctx) }

      // Here we figure out what the transition to the RunningState should be,
      // assuming we aren't already observed and that the hash differs.
      val runningTransition = for {
        (c, s) <- tup
        h      <- newHash
        if !hash.contains(h) && notObserved
      } yield (RunningState(obs, h), BagsManager.triggerAgsAction(obs)): StateTransition

      // Returns the new state to switch to, either RunningState if all is well
      // and a new AGS lookup is needed or else IdleState (possibly with an
      // updated hash value) otherwise.
      def idleAction = tup.fold(BagsManager.clearAction(obs)) { _ => ioUnit }
      runningTransition | ((IdleState(obs, newHash), idleAction))
    }
  }

  /** Running.  The running state means we're doing an AGS lookup and haven't
    * received the results yet.  We also know that there have been no edits
    * since the search began since we will transition to RunningEditedState
    * if something is edited in the meantime.
    */
  case class RunningState(obs: ISPObservation, hash: AgsHashVal) extends BagsState {
    override val status: BagsStatus = BagsStatus.Running

    // When edited while running, we continue running but remember we were
    // edited by moving to RunningEditedState.  When the results eventually
    // come back from the AGS lookup when in RunningEditedState, we store them
    // but move to PendingState to run again.
    override val edit: StateTransition =
      (RunningEditedState(obs, hash), ioUnit)

    // Successful AGS lookup while running (and not edited).  Apply the update
    // and move to IdleState.
    override def succeed(results: Option[AgsStrategy.Selection]): StateTransition =
      (IdleState(obs, Some(hash)), BagsManager.applyAction(obs, results))

    // Failed AGS lookup.  Move to FailedState for a while and ask the timer to
    // wake us up in a bit.  When the timer eventually goes off we will switch
    // to PendingState (see FailedState.wakeUp).
    override def fail(why: String, delayMs: Long): StateTransition =
      (FailureState(obs, why), BagsManager.wakeUpAction(obs, delayMs))
  }

  /** RunningEdited. This state corresponds to a running AGS search for an
    * observation that was subsequently edited.  Since it has been edited, the
    * resuls we're expecting may no longer be valid when they arrive.
    */
  case class RunningEditedState(obs: ISPObservation, hash: AgsHashVal) extends BagsState {
    override val status: BagsStatus = BagsStatus.Running

    // If we're edited again while running, just loop back.  Once the results of
    // the previous edit that got us into RunningState in the first place
    // finish, we'll switch to Pending and go again.
    override val edit: StateTransition =
      (this, ioUnit)

    // Success, but now the observation has been edited so the AGS selection
    // may not be valid.  Apply the results anyway in case the edit is not
    // to a field that impacts AGS and go to pending to run again if necessary.
    override def succeed(results: Option[AgsStrategy.Selection]): StateTransition = {
      val action = for {
        _ <- BagsManager.applyAction(obs, results)
        _ <- BagsManager.wakeUpAction(obs, 0)
      } yield ()
      (PendingState(obs, Some(hash)), action)
    }

    // Failed AGS but we've been edited in the meantime anyway.  Go back to
    // PendingState so we can run again.  Here we pass in no AgsHash value to
    // ensure that pending will count this as an AGS-worthy edit.
    override def fail(why: String, delayMs: Long): StateTransition =
      (PendingState(obs, None), BagsManager.wakeUpAction(obs, delayMs))
  }

  /** Failure. This state is used to mark an AGS lookup failure.  It is a
    * transient state since a timer is expected to eventually go off and move
    * us back to pending to retry the search.
    */
  case class FailureState(obs: ISPObservation, why: String) extends BagsState {
    override def status: BagsStatus = BagsStatus.Failed(why)

    // If edited while in failed, we just loop back.  When the timer eventually
    // goes off we'll move to pending and redo the AGS lookup anyway.
    override val edit: StateTransition =
      (this, ioUnit)

    // When the timer goes off we can go back to pending (with no AgsHash value
    // since we want to ensure that the AGS search runs again).
    override val wakeUp: StateTransition =
      (PendingState(obs, None), BagsManager.wakeUpAction(obs, 0))
  }


  private def hashObs(obs: ISPObservation, ctx: ObsContext): AgsHashVal = {
    val when = obs.getDataObject.asInstanceOf[SPObservation].getSchedulingBlock.asScalaOpt.map(_.start) | Instant.now.toEpochMilli
    AgsHash.hash(ctx, when)
  }

  // Performs checks to rule out disabled guide groups, instruments without
  // guiding strategies (e.g. GPI), observation classes that do not have
  // guiding (e.g. daytime calibration).
  private def agsStrategy(obs: ISPObservation, ctx: ObsContext): Option[AgsStrategy] = {
    val groupEnabled = ctx.getTargets.getGuideEnvironment.guideEnv.auto match {
      case AutomaticGroup.Initial | AutomaticGroup.Active(_, _) => true
      case _                                                    => false
    }

    AgsRegistrar.currentStrategy(ctx).filter { _ =>
      groupEnabled && (ObsClassService.lookupObsClass(obs) != ObsClass.DAY_CAL)
    }
  }
}


object BagsManager {
  private val Log = Logger.getLogger(getClass.getName)

  // Worker pool running the AGS search and the timer.
  val NumWorkers = math.max(1, Runtime.getRuntime.availableProcessors - 1)
  private val worker = new ScheduledThreadPoolExecutor(NumWorkers, new ThreadFactory() {
    override def newThread(r: Runnable): Thread = {
      val t = new Thread(r, "BagsManager - Worker")
      t.setPriority(Thread.NORM_PRIORITY - 1)
      t.setDaemon(true)
      t
    }
  })
  private implicit val executionContext = ExecutionContext.fromExecutor(worker)

  private implicit val OrderSPProgramID: Order[SPProgramID] =
    Order.order((p0, p1) => Ordering.fromInt(p0.compareTo(p1)))

  private implicit val OrderSPNodeKey: Order[SPNodeKey] =
    Order.order((k0, k1) => Ordering.fromInt(k0.compareTo(k1)))

  // This is our mutable state.  It is only read/written by the Swing thread.
  // TODO: Should be SPNodeKey ==>> SPNodeKey ==>> BagsState since not all
  // TODO: programs have an SPProgramID.  We might want to make a value class
  // TODO: to wrap SPNodeKey and give a new type for programs and observations
  private var stateMap  = ==>>.empty[SPProgramID, SPNodeKey ==>> BagsState]
  private var listeners = Set.empty[BagsStatusListener]

  // Handle a state machine transition.  Note we switch to the Swing thread
  // here so that the calling thread terminates.  All updates are done from
  // the Swing thread.
  private def transition(obs: ISPObservation, update: BagsState => StateTransition): Unit = Swing.onEDT {
    val key = obs.getNodeKey

    val stateTuple = for {
      pid    <- Option(obs.getProgramID)
      obsMap <- stateMap.lookup(pid)
      state  <- obsMap.lookup(key)
    } yield (pid, obsMap, state)

    stateTuple.foreach { case (pid, obsMap, state) =>
      val (newState, sideEffect) = update(state)
      val newObsMap              = obsMap.insert(key, newState)
      val newStatemap            = stateMap.insert(pid, newObsMap)

      val oldStatus = state.status
      val newStatus = newState.status

      val action = for {
        _ <- IO { stateMap = newStatemap }
        _ <- sideEffect
        _ <- (oldStatus == newStatus) ? ioUnit | IO { notify(key, oldStatus, newStatus) }
      } yield ()

      action.unsafePerformIO()
    }
  }

  private def edited(obs: ISPObservation): Unit =
    transition(obs, _.edit)

  private def wakeUp(obs: ISPObservation): Unit =
    transition(obs, _.wakeUp)

  private def success(obs: ISPObservation, results: Option[AgsStrategy.Selection]): Unit =
    transition(obs, _.succeed(results))

  private def fail(obs: ISPObservation, why: String, delay: Long): Unit =
    transition(obs, _.fail(why, delay))

  /** BagsStatus lookup for the UI. */
  def bagsStatus(o: ISPObservation): Option[BagsStatus] =
    for {
      pid <- Option(o.getProgram).map(_.getProgramID)
      s   <- bagsStatus(pid, o.getNodeKey)
    } yield s

  /** BagsStatus lookup for the UI. */
  def bagsStatus(pid: SPProgramID, key: SPNodeKey): Option[BagsStatus] = {
    for {
      m <- stateMap.lookup(pid)
      s <- m.lookup(key)
    } yield s.status
  }

  /** Add a program to our watch list and attach listeners. Enqueue all
    * observations for consideration for a BAGS lookup.
    */
  def watch(prog: ISPProgram): Unit = {
    // TODO: Here we should probably check that the program isn't being
    // TODO: managed already.  If it is, we should do nothing!

    // TODO: I removed structure change listener since the composite change
    // TODO: listener adds any missing observations it finds.  I'm not sure
    // TODO: that is sufficient though.

    prog.addCompositeChangeListener(CompositePropertyChangeListener)

    // Place all observations in the IdleState and record them in our stateMap.
    val obsList = prog.getAllObservations.asScala.toList
    val obsMap  = ==>>.fromList(obsList.map(o => o.getNodeKey -> (IdleState(o, None): BagsState)))

    Option(prog.getProgramID).foreach { pid =>
      stateMap = stateMap + (pid -> obsMap)
    }

    // Now mark all the obsevations "edited" in order to kick off AGS searches
    // and hash calculations.
    obsList.foreach(edited)
  }

  /** Remove a program from our watch list and remove listeners.
    */
  def unwatch(prog: ISPProgram): Unit = {
    prog.removeCompositeChangeListener(CompositePropertyChangeListener)

    Option(prog.getProgramID).foreach { pid =>
      stateMap = stateMap - pid
    }
  }

  private def enqueue(o: ISPObservation): Unit =
    Option(o.getProgramID).foreach { pid =>
      val obsMap  = stateMap.lookup(pid).getOrElse(==>>.empty[SPNodeKey, BagsState])
      val obsMap2 = obsMap.alter(o.getNodeKey, {
        case None => Some(IdleState(o, None))
        case x    => x
      })
      stateMap = stateMap + (pid -> obsMap2)
      BagsManager.edited(o)
    }

  object CompositePropertyChangeListener extends PropertyChangeListener {
    override def propertyChange(evt: PropertyChangeEvent): Unit =
      evt.getSource match {
        case node: ISPNode => Option(node.getContextObservation).foreach(enqueue)
        case _             => // Ignore
      }
  }


  def addBagsStatusListener(l: BagsStatusListener): Unit = {
    listeners = listeners + l
  }

  def removeBagsStatusListener(l: BagsStatusListener): Unit = {
    listeners = listeners - l
  }

  def clearBagsStateListeners(): Unit = {
    listeners = Set.empty
  }

  private def notify(key: SPNodeKey, oldStatus: BagsStatus, newStatus: BagsStatus): Unit =
    listeners.foreach(l => Try { l.bagsStatusChange(key, oldStatus, newStatus) })


  // -------------------------------------------------------------------------
  // Side effects that accompany the state transitions.
  // -------------------------------------------------------------------------

  private[ags] def wakeUpAction(obs: ISPObservation, delayMs: Long): IO[Unit] = IO {
    worker.schedule(new Runnable() {
      def run(): Unit = BagsManager.wakeUp(obs)
    }, delayMs, TimeUnit.MILLISECONDS)
  }

  private[ags] def triggerAgsAction(obs: ISPObservation): IO[Unit] = IO {
    val bagsIdMsg = s"BAGS lookup on thread=${Thread.currentThread.getId} for observation=${obs.getObservationID}"
    Log.info(s"Performing $bagsIdMsg.")

    val fut = for {
      ctx <- ObsContext.create(obs).asScalaOpt
      ags <- AgsRegistrar.currentStrategy(ctx)
    } yield ags.select(ctx, OT.getMagnitudeTable)

    fut.fold(BagsManager.fail(obs, "Cannot perform AGS on this observation", 0)) { f =>
      f.onComplete {
        case Success(opt) =>
          // TODO: Sleep just to show that it is working.  Otherwise the
          // TODO: transition from Pending through Running to Idle is immediate
          // TODO: in most cases.  Remove once satisfied that all is well.
          Thread.sleep(2000)
          BagsManager.success(obs, opt)

        case Failure(CatalogException((e: GenericError) :: _)) =>
          BagsManager.fail(obs, "Catalog lookup failed.", 5000)

        case Failure(ex: TimeoutException) =>
          BagsManager.fail(obs, "Catalog timed out.", 0)

        case Failure(ex) =>
          BagsManager.fail(obs, s"Unexpected error ${Option(ex.getMessage).getOrElse("")}", 5000)
      }
    }
  }

  private[ags] def clearAction(obs: ISPObservation): IO[Unit] =
    applyAction(obs, None)

  private[ags] def applyAction(obs: ISPObservation, selOpt: Option[AgsStrategy.Selection]): IO[Unit] = IO {
    def applySelection(ctx: TpeContext): Unit = {
      val oldEnv = ctx.targets.envOrDefault

      // If AGS results were found, apply them to the target env; otherwise, clear out any existing auto group.
      val newEnv = selOpt.map(_.applyTo(oldEnv)).getOrElse {
        val oldGuideEnv = oldEnv.getGuideEnvironment.guideEnv
        if (oldGuideEnv.auto === AutomaticGroup.Initial || oldGuideEnv.auto === AutomaticGroup.Disabled) oldEnv
        else oldEnv.setGuideEnvironment(GuideEnvironment(GuideEnv(AutomaticGroup.Initial, oldGuideEnv.manual)))
      }

      // Calculate the new target environment and if they are different referentially, apply them.
      ctx.targets.dataObject.foreach { targetComp =>
        // If the env reference hasn't changed, this does nothing.
        if (oldEnv != newEnv) {
          targetComp.setTargetEnvironment(newEnv)
          ctx.targets.commit()
        }

        // Change the pos angle as appropriate if this is the auto group.
        selOpt.foreach { sel =>
          if (newEnv.getPrimaryGuideGroup.isAutomatic && selOpt.isDefined) {
            ctx.instrument.dataObject.foreach { inst =>
              val deg = sel.posAngle.toDegrees
              val old = inst.getPosAngleDegrees
              if (deg != old) {
                inst.setPosAngleDegrees(deg)
                ctx.instrument.commit()
              }
            }
          }
        }
      }
    }

    // TODO: if we add back a structure change listener, i suppose it needs
    // TODO: to be included here to
    obs.getProgram.removeCompositeChangeListener(CompositePropertyChangeListener)
    applySelection(TpeContext(obs))

    // Update the TPE if it is visible
    selOpt.foreach { sel =>
      Option(TpeManager.get()).filter(_.isVisible).foreach { tpe =>
        sel.assignments.foreach { ass =>
          val clazz = ass.guideProbe.getType match {
            case GuideProbe.Type.AOWFS => classOf[Altair_WFS_Feature]
            case GuideProbe.Type.OIWFS => classOf[OIWFS_Feature]
            case GuideProbe.Type.PWFS  => classOf[TpePWFSFeature]
          }
          Option(tpe.getFeature(clazz)).foreach(tpe.selectFeature)
        }
      }
    }

    obs.getProgram.addCompositeChangeListener(CompositePropertyChangeListener)
  }
}