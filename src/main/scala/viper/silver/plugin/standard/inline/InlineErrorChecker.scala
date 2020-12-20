package viper.silver.plugin.standard.inline

import viper.silver.ast._

trait InlineErrorChecker {

  /**
    * For each predicate id, find its associated body in the program. If the predicate is recursive,
    * print a warning to the console that it shall not be inlined. Return a set of predicates that
    * are recursive.
    *
    * @param predicateIds the ids of the predicates we want to inline.
    * @param program the program for which we are performing predicate inlining on.
    * @return the set of recursive predicates.
    */
  def checkRecursive(predicateIds: Set[String], program: Program): Set[Predicate] = {
    val predicates = predicateIds.map(program.findPredicate)
    val recursivePreds = predicates.filter {
          case Predicate(name, _, maybeBody) => isRecursivePred(name, maybeBody)
    }
    if (recursivePreds.nonEmpty) {
      prettyPrint(recursivePreds, "recursive")
    }
    recursivePreds
  }

  /**
    * Construct a call-graph of predicates specified by the given predicate ids. If any predicates
    * are found to be mutually-recursive, print a warning to the console that it shall not be inlined.
    * Return a set of predicates that are mutually-recursive.
    *
    * @param predicateIds the ids of the predicates we want to inline.
    * @param program the program for which we are performing predicate inlining on.
    * @return the set of mutually-recursive predicates.
    */
  def checkMutualRecursive(predicateIds: Set[String], program: Program): Set[Predicate] = {
    val predicatesToInspect = predicateIds.map(program.findPredicate)
    val predicateCallGraph = PredicateCallGraph.graph(predicatesToInspect, program)
    val mutRecPreds = PredicateCallGraph.mutuallyRecursivePreds(predicateCallGraph)
    if (mutRecPreds.nonEmpty) {
      prettyPrint(mutRecPreds, "mutually recursive")
    }
    mutRecPreds
  }

  /**
    * Given a predicate, evaluate to a set containing the names of all predicates called by it.
    * Only return the names of predicates called that aren't equal to itself.
    *
    * @param pred the predicate we want to collect the names of inner predicate calls for.
    * @return a set of the ids of predicates called in the given predicate.
    */
  def nonRecursivePredsCalledBy(pred: Predicate): Option[Set[String]] =
    pred.body.map { body =>
      val children = body.subnodes
      // Forgive me Father Alonzo for I have Sinned
      var calledPreds = Set[String]()
      children.foreach { child =>
        child.visit {
          case PredicateAccessPredicate(calledPred, _) =>
            if (calledPred.predicateName != pred.name)
              calledPreds += calledPred.predicateName
        }
      }
      calledPreds
    }

  /**
    * Given a predicate id and possibly its body, search the body for a node of type
    * PredicateAccessPredicate(...) with the name identical to the predicate id.
    * If such a node is found, the predicate is recursively defined.
    *
    * @param predId the id for the predicate we want to check for a recursive definition.
    * @param maybePredBody the possible body of the predicate.
    * @return true iff the predicate is found to be recursively defined.
    */
  private[this] def isRecursivePred(predId: String, maybePredBody: Option[Node]): Boolean =
    maybePredBody.fold(false) { predBody =>
      val subNodes = predBody.subnodes
      val existsAtTopLevelNode = subNodes.exists {
        case PredicateAccessPredicate(PredicateAccess(_, name), _) => name == predId
        case _ => false
      }
      lazy val isInChildNodes = subNodes.exists(child => isRecursivePred(predId, Some(child)))
      existsAtTopLevelNode || isInChildNodes
    }

  private[this] def prettyPrint(preds: Set[Predicate], errorReason: String): Unit = {
    val predIds = preds.map(_.name).mkString(", ")
    if (preds.size > 1) {
      println(s"[$predIds] are $errorReason predicates and will not be inlined.")
    } else {
      println(s"[$predIds] is a $errorReason predicate and will not be inlined.")
    }
  }
}
