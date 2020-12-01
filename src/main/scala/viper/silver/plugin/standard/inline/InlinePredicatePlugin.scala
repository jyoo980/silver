package viper.silver.plugin.standard.inline

import viper.silver.ast.Program
import viper.silver.plugin.{ParserPluginTemplate, SilverPlugin}

class InlinePredicatePlugin extends SilverPlugin with ParserPluginTemplate with InlineRewrite {

  private[this] val InlinePredicateKeyword = "inline"

  override def beforeVerify(input: Program): Program = {
     val inlinedPredicateMethods = input.methods.map(inlinePredicates(_, input))

    println(s"METHODS BEFORE")
    println(s"${input.methods}")
    println(s"METHODS AFTER")
    println(s"$inlinedPredicateMethods")

//
//    val newProgram: Program = ViperStrategy.Slim({
//      case p: Program =>
//        p.copy(predicates = Seq())(p.pos, p.info, p.errT)
//    }, Traverse.BottomUp).execute(input)
//    println(s"After removing predicates: $newProgram")
    input
  }
}
