package edu.gemini.sp.vcs.diff

import edu.gemini.pot.sp.{ISPFactory, SPNodeKeyLocks, ISPProgram, SPNodeKey}
import edu.gemini.pot.sp.version.VersionMap
import edu.gemini.pot.spdb.{DBIDClashException, IDBDatabaseService}
import edu.gemini.shared.util.VersionComparison.{Same, Newer}
import edu.gemini.sp.vcs.diff.VcsAction._
import edu.gemini.sp.vcs.diff.VcsFailure._
import edu.gemini.sp.vcs.log.{OpStore, OpFetch, VcsEventSet, VcsLog}
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.util.security.permission.ProgramPermission
import edu.gemini.util.security.policy.ImplicitPolicy

import java.security.Principal

import edu.gemini.util.security.principal.GeminiPrincipal

import scalaz._
import Scalaz._

/** VcsServer provides access-controlled locked read/write methods to generate
  * actions that read from and write to programs in the database. It also
  * implements the server side of the [[edu.gemini.sp.vcs.diff.VcsService]]
  * interface. */
class VcsServer(odb: IDBDatabaseService, vcsLog: VcsLog) { vs =>

  import SPNodeKeyLocks.instance.{readLock, readUnlock, writeLock, writeUnlock}

  /** Creates an action that reads from a program with a read lock held
    * provided the caller has permission. */
  def read[A](id: SPProgramID, user: Set[Principal])(body: ISPProgram => A): VcsAction[A] =
    locking(id, user, readLock, readUnlock)(p => body(p).point[VcsAction])

  /** Creates an action that potentially writes to a program with a write lock
    * held provided the caller has permission.  Writing is done on a copy of the
    * associated program and only when successful the copy replaces the current
    * version of the program in the database.
    *
    * The caller provides three functions, `evaluate`, `filter`, and `update`.
    * Evaluate should not modify anything in the progrm but rather calculate
    * a value.  The value is handed to `filter` to decide whether to continue
    * with the write operation (and make the program copy).  In either case,
    * the value is returned as the result of the action.  The `update` takes a
    * copy of the program and the value returned by `evaluate` and mutates the
    * copy.  If successful, the copy replaces the current version of the program
    * in the database.
    *
    * @param id program id
    * @param user authentication
    * @param evaluate a function to compute a value which is used to determine
    *                 whether to continue the write operation
    * @param filter a function that takes the value returned by the `evaluate`
    *               function and returns `true` if the write should continue
    * @param update a function that performs the side effect of updating the
    *               program copy
    * @return the value produced by the `evaluate` function
    */
  def write[A](id:       SPProgramID,
               user:     Set[Principal],
               evaluate: ISPProgram => VcsAction[A],
               filter:   A => Boolean,
               update:   (ISPFactory, ISPProgram, A) => VcsAction[Unit]): VcsAction[A] = {

    def storeCopy(copy: ISPProgram, orig: ISPProgram): TryVcs[Unit] = {
      \/.fromTryCatch(odb.put(copy)).leftMap {
        case clash: DBIDClashException =>
          KeyClash(orig.getProgramKey, orig.getProgramID, clash.key, clash.id)
        case ex =>
          VcsException(ex)
      }.map(_ => ())
    }

    locking(id, user, writeLock, writeUnlock) { prog =>
      evaluate(prog) >>= { a =>
        if (filter(a)) {
          val cp = odb.getFactory.copyWithSameKeys(prog)
          update(odb.getFactory, cp, a) >>= { _ => storeCopy(cp, prog).map(_ => a).liftVcs }
        } else {
          VcsAction(a)
        }
      }
    }
  }

  /** Server implementation of `VcsService`. */
  final class SecureVcsService(user: Set[Principal]) extends VcsService {
    def geminiPrincipals: Set[GeminiPrincipal] =
      user.collect { case p: GeminiPrincipal => p }

    override def version(id: SPProgramID): TryVcs[VersionMap] =
      vs.read(id, user)(_.getVersions).unsafeRun

    override def diffState(id: SPProgramID): TryVcs[DiffState] =
      vs.read(id, user)(DiffState.apply).unsafeRun

    override def fetchDiffs(id: SPProgramID, state: DiffState): TryVcs[MergePlan.Transport] =
      vs.read(id, user) { p =>
        vcsLog.log(OpFetch, id, geminiPrincipals)
        ProgramDiff.compare(p, state)
      }.map(_.encode).unsafeRun

    override def storeDiffs(id: SPProgramID, mpt: MergePlan.Transport): TryVcs[Boolean] = {
      val mp = mpt.decode
      vs.write[Boolean](id, user,
        p => VersionMap.compare(mp.vm(p), p.getVersions) match {
          case Newer => VcsAction(true)
          case Same  => VcsAction(false)
          case _     => VcsAction.fail(NeedsUpdate)
        },
        identity,
        (f, p, _) =>
          for {
            _ <- mp.merge(f, p)
            _ <- VcsAction(vcsLog.log(OpStore, id, geminiPrincipals))
          } yield ()
      ).unsafeRun
    }

    override def log(id: SPProgramID, offset:Int, length:Int): TryVcs[(List[VcsEventSet], Boolean)] =
      try {
        vcsLog.selectByProgram(id, offset, length).right
      } catch {
        case ex: Exception => VcsException(ex).left
      }
  }

  private def locking[A](id: SPProgramID, user: Set[Principal], lock: SPNodeKey => Unit, unlock: SPNodeKey => Unit)(body: ISPProgram => VcsAction[A]): VcsAction[A] =
    accessControlled(id, user) {
      lookup(id) >>= { p =>
        lock(p.getNodeKey)
        try { body(p) } finally { unlock(p.getNodeKey) }
      }
    }

  private def lookup(id: SPProgramID): VcsAction[ISPProgram] =
    (Option(odb.lookupProgramByID(id)) \/> NotFound(id)).liftVcs

  private def accessControlled[A](id: SPProgramID, user: Set[Principal])(body: => VcsAction[A]): VcsAction[A] =
    VcsAction(ImplicitPolicy.hasPermission(odb, user, new ProgramPermission.Read(id)).unsafePerformIO()) >>= {
      hasPermission =>
        if (hasPermission) body
        else VcsAction.fail(Forbidden(s"You don't have permission to access program '$id'"))
    }
}