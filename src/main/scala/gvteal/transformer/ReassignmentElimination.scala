package gvc.transformer

import scala.collection.immutable.ListMap

// This transformation removes cases of `x = f(x)`, because Silver does not support this pattern.
// The basic idea is to assign to a new variable, and then use that variable anywhere the original
// variable is used in the following code. This is somewhat challenging when dealing with branches
// (if) and loops (while).
object ReassignmentElimination {
  def transform(method: IR.Method): Unit = {
    val reassign = new Reassignments(method)
    method.body.foreach(replace(_, reassign))
  }

  private class Reassignments(
    method: IR.Method,
    var mappings: Map[IR.Var, IR.Var] = Map.empty,
    var created: ListMap[IR.Var, IR.Var] = ListMap.empty,
    var remaining: ListMap[IR.Var, IR.Var] = ListMap.empty,
  ) {
    def reassign(v: IR.Var): IR.Var = {
      val newV = remaining.get(v) match {
        case Some(newV) => {
          remaining -= v
          newV
        }
        case None => method.addVar(v.varType, v.name)
      }

      created += v -> newV
      mappings += v -> newV
      newV
    }

    def assign(v: IR.Var): IR.Var =
      remaining.get(v)
        .map(newV => {
          remaining -= v
          created += v -> newV
          mappings += v -> newV
          newV
        })
        .getOrElse(v)

    def copy(created: ListMap[IR.Var, IR.Var] = ListMap.empty, remaining: ListMap[IR.Var, IR.Var] = ListMap.empty) =
      new Reassignments(method = method, mappings = mappings, created = created, remaining = remaining)
  }

  private def replace(op: IR.Op, reassign: Reassignments): Unit = {
    Replacer.replaceShallow(op, reassign.mappings)

    op match {
      case inv: IR.Invoke => {
        inv.target = inv.target.map {
          case v: IR.Var if inv.arguments.exists(_.contains(v)) =>
            reassign.reassign(v)
          case other => other
        }
      }

      case assign: IR.Assign => {
        // Attempt to minimize the number of "patched" assignments by using
        // the variable from the remaining list if one exists
        assign.target = reassign.assign(assign.target)
      }

      case iff: IR.If => {
        val trueReassign = reassign.copy(remaining = reassign.remaining)
        iff.ifTrue.foreach(replace(_, trueReassign))

        val falseReassign = reassign.copy(remaining = reassign.remaining ++ trueReassign.created)
        iff.ifFalse.foreach(replace(_, falseReassign))

        // Any mappings created in the false branch and not in the true branch
        // are "patched up" by aliasing in the true branch
        (falseReassign.created -- trueReassign.created.keys)
          .foreach { case (oldV, newV) => iff.ifTrue += new IR.Assign(newV, oldV) }
        // Same thing, but for the false branch
        (trueReassign.created -- falseReassign.created.keys)
          .foreach { case (oldV, newV) => iff.ifFalse += new IR.Assign(newV, oldV) }

        val created = trueReassign.created ++ falseReassign.created
        reassign.created = reassign.created ++ created
        reassign.remaining = reassign.remaining -- created.keys
        reassign.mappings = reassign.mappings ++ created
      }

      case loop: IR.While => {
        // Any mappings created inside the loop body are "undone" at the end of the loop
        // body, thus code after the loop body (the conditional, following code, or the
        // next loop iteration) is unaffected.

        val bodyReassign = reassign.copy()
        loop.body.foreach(replace(_, bodyReassign))

        bodyReassign.created.foreach {
          case (oldV, newV) => loop.body += new IR.Assign(oldV, newV)
        }
      }

      case _ => ()
    }
  }
}