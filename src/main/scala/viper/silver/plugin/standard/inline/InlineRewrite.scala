package viper.silver.plugin.standard.inline

import viper.silver.ast._

trait InlineRewrite {

  def collectExpandablePredicateIds(methods: Seq[Method], program: Program): Seq[String] = {
    def expandablePredicatesFor(method: Method): Seq[String] =
      (method.pres ++ method.posts).collect {
        case PredicateAccessPredicate(pred, _)
          if getPredicateBody(pred, program).nonEmpty => pred.predicateName
      }
    methods.foldLeft(Seq[String]())(_ ++ expandablePredicatesFor(_))
  }

  def inlinePredicates(
    method: Method,
    program: Program
  ): Method = {
    val expandedPres = method.pres.map { pre =>
      expandPredicate(pre, program).fold(pre)(expandedPred => expandedPred)
    }
    // TODO: test postcondition expansion works
    // val expandedPosts = method.posts.map { post =>
    //  expandPredicate(post, program).fold(post)(expandedPred => expandedPred)
    // }
    method.copy(name = method.name,
      formalArgs = method.formalArgs,
      formalReturns = method.formalReturns,
      pres = expandedPres,
      posts = method.posts,
      body = method.body
    )(pos = method.pos, info = method.info, errT = method.errT)
  }

  def removeUnfoldFold(method: Method, expandablePredIds: Seq[String]): Method = {
    val rewrittenBody = method.body.map { body =>
      val bodyWithRemovedUnfolds = removeUnfolds(body.ss, expandablePredIds)
      body.copy(ss = bodyWithRemovedUnfolds, scopedDecls = body.scopedDecls)(
        pos = body.pos, info = body.info, errT = body.errT
      )
      val bodyWithRemovedUnfoldFolds = removeFolds(bodyWithRemovedUnfolds, expandablePredIds)
      body.copy(ss = bodyWithRemovedUnfoldFolds, scopedDecls = body.scopedDecls)(
        pos = body.pos, info = body.info, errT = body.errT
      )
    }
    method.copy(name = method.name,
      formalArgs = method.formalArgs,
      formalReturns = method.formalReturns,
      pres = method.pres,
      posts = method.posts,
      body = rewrittenBody
    )(pos = method.pos, info = method.info, errT = method.errT)
  }

  private[this] def expandPredicate(expr: Exp, program: Program): Option[Exp] =
    expr match {
        case PredicateAccessPredicate(pred, _) =>
          getPredicateBody(pred, program)
        case _ => None
    }

  private[this] def getPredicateBody(pred: PredicateAccess, program: Program): Option[Exp] = {
    val localVarIds = pred.args.collect {
      case LocalVar(name, _) => name
    }.toSet
    pred.predicateBody(program, localVarIds)
  }

  private[this] def removeUnfolds(bodyStmts: Seq[Stmt], predicatesToRemove: Seq[String]): Seq[Stmt] = {
    def filterUnfolds(seqn: Seqn): Seqn = {
      val seqnNoUnfolds = seqn.ss.filter {
        case Unfold(pred) => !predicatesToRemove.contains(pred.loc.predicateName)
        case _ => true
      }
      seqn.copy(ss = seqnNoUnfolds, scopedDecls = seqn.scopedDecls)(
        pos = seqn.pos,
        info = seqn.info,
        errT = seqn.errT
      )
    }
    bodyStmts.map {
      case s: Seqn => filterUnfolds(s)
      case other => other
    }
  }

  private[this] def removeFolds(bodyStmts: Seq[Stmt], predicatesToRemove: Seq[String]): Seq[Stmt] = {
    def filterFolds(seqn: Seqn): Seqn = {
      val seqnNoUnfolds = seqn.ss.filter {
        case Fold(acc) => !predicatesToRemove.contains(acc.loc.predicateName)
        case _ => true
      }
      seqn.copy(ss = seqnNoUnfolds, scopedDecls = seqn.scopedDecls)(
        pos = seqn.pos,
        info = seqn.info,
        errT = seqn.errT
      )
    }
    bodyStmts.map {
      case s: Seqn => filterFolds(s)
      case other => other
    }
  }
}
