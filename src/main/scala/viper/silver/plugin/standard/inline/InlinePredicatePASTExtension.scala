package viper.silver.plugin.standard.inline

import viper.silver.parser._
import viper.silver.ast.{Member, Predicate}

sealed trait PInlinePredicateDeclaration extends PExtender with PMember with PGlobalDeclaration {}

case class PInlinePredicate(predicate: PPredicate) extends PInlinePredicateDeclaration {
  override def idndef: PIdnDef = predicate.idndef
  override def getSubnodes(): Seq[PNode] = Seq(predicate)
  override def typecheck(t: TypeChecker, n: NameAnalyser): Option[Seq[String]] = None // TODO

  override def translateMember(t: Translator): Member = {
    val name = predicate.idndef.name
    val formalArgs = predicate.formalArgs.map(t.liftVarDecl)
    val maybeBody = predicate.body.map(t.exp)
    InlinePredicate(
      Predicate(name, formalArgs, maybeBody)() // TODO: not too sure about the curried argument list
    )(t.liftPos(predicate))
  }
}
