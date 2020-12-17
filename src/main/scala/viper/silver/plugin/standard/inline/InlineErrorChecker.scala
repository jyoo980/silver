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
    * For each predicate id, find its associated body in the program. If the predicate is nested,
    * print a warning to the console that it shall not be inlined. Return a set of predicates that
    * are recursive
    *
    * @param predicateIds the ids of the predicates we want to inline.
    * @param program the program for which we are performing predicate inlining on.
    * @return the set of nested predicates.
    */
  def checkNestedPreds(predicateIds: Set[String], program: Program): Set[Predicate] = {
    val predicates = predicateIds.map(program.findPredicate)
    val nestedPreds = predicates.filter {
      case Predicate(name, _, _) => isNestedPred(name, predicates)
    }
    if (nestedPreds.nonEmpty) {
      prettyPrint(nestedPreds, "nested")
    }
    nestedPreds
  }

  /**
    * Given a predicate id and a list of other predicates, for every predicate in the list of other predicates,
    * check if the predicate id exists within the predicate
    *
    * @param predId the id for the predicate we want to check for a nested definition.
    * @param otherPredicates the list of predicates we want to search for predicate id in.
    * @return true iff the predicate id is found to in any of the predicates in otherPredicates.
    */
  private[this] def isNestedPred(predId: String, otherPredicates: Set[Predicate]): Boolean = {
    val filteredOtherPreds = otherPredicates.filterNot(_.name == predId)
    val filteredOtherPredBodies = filteredOtherPreds.flatMap(_.body)
    val nestedWithinFirst = filteredOtherPredBodies.exists {
      case PredicateAccessPredicate(PredicateAccess(_, predName), _) => predName == predId
      case _ => false
    }
    lazy val isInChildExpr = filteredOtherPredBodies.exists(findWithinExpr(predId, _))
    nestedWithinFirst || isInChildExpr
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

  /**
    * Given a predicate id and an expression, check whether the predicate id is found within
    * the expression or any of its child expressions.
    *
    * @param predId the id for the predicate we want to check for a nested definition.
    * @param expr the expression which we want to check the existence of the predId
    * @return true iff the expression or any of its children contains the id
    */
  private[this] def findWithinExpr(predId: String, expr: Node): Boolean =
    if (expr.subnodes.isEmpty) false
    else
      expr match {
        case PredicateAccessPredicate(PredicateAccess(_, otherPredName), _) => otherPredName == predId
        case otherExpr => otherExpr.subnodes.exists(findWithinExpr(predId, _))
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
