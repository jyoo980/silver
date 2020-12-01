package viper.silver.plugin.standard.inline

import viper.silver.ast.Program
import viper.silver.plugin.{ParserPluginTemplate, SilverPlugin}

class InlinePredicatePlugin extends SilverPlugin with ParserPluginTemplate with InlineRewrite {

  private[this] val InlinePredicateKeyword = "inline"

  override def beforeVerify(input: Program): Program = {
    val expandablePredicateIds = collectExpandablePredicateIds(input.methods, input)
    val inlinedPredicateMethods = input.methods.map(inlinePredicates(_, input))
    val foldUnfoldRemoved = inlinedPredicateMethods.map(removeUnfoldFold(_, expandablePredicateIds))

    println(s"METHODS BEFORE INLINING")
    println(s"${input.methods}")
    println(s"METHODS AFTER INLINING")
    println(s"$foldUnfoldRemoved")

//
//    val newProgram: Program = ViperStrategy.Slim({
//      case p: Program =>
//        p.copy(predicates = Seq())(p.pos, p.info, p.errT)
//    }, Traverse.BottomUp).execute(input)
//    println(s"After removing predicates: $newProgram")
    input
  }
}
