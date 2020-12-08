package viper.silver.plugin.standard.inline

import fastparse.noApi
import viper.silver.ast.utility.ViperStrategy
import viper.silver.ast.Program
import viper.silver.plugin.{ParserPluginTemplate, SilverPlugin}
import viper.silver.parser.ParserExtension
import viper.silver.parser.FastParser._
import viper.silver.parser._
import White._

class InlinePredicatePlugin extends SilverPlugin with ParserPluginTemplate
  with InlineRewrite
  with InlineErrorChecker {
  import fastparse.noApi._

  private[this] val InlinePredicateKeyword = "inline"

  lazy val inlinePredicate: noApi.P[PInlinePredicate] =
    P(keyword(InlinePredicateKeyword) ~/ predicateDecl).map(pred => PInlinePredicate(pred))

  override def beforeParse(input: String, isImported: Boolean): String = {
    ParserExtension.addNewKeywords(Set[String](InlinePredicateKeyword))
    ParserExtension.addNewDeclAtEnd(inlinePredicate)
    input
  }

  override def beforeVerify(input: Program): Program = {
    val rewrittenMethods = input.methods.map { method =>
      val inlinePredIds = input.extensions.collect({case InlinePredicate(p) => p.name}).toSet
      checkRecursive(inlinePredIds, input)
      // TODO: Do we also need to inline in inhale/exhale/assert/assume and package/apply statements?
      val (prePredIds, postPredIds) = getPrePostPredIds(method, input, inlinePredIds)
      val inlinedPredMethod = inlinePredicates(method, input, prePredIds, postPredIds)
      rewriteMethod(inlinedPredMethod, input, prePredIds, postPredIds)
    }
    // TODO: Do we also need to rewrite functions?
    val rewrittenProgram = ViperStrategy.Slim({
      case program@Program(_, _, _, predicates, _, extensions) =>
        program.copy(
          methods = rewrittenMethods,
          predicates = predicates ++ extensions.collect{case InlinePredicate(p) => p},
        )(program.pos, program.info, program.errT)
    }).execute[Program](input)

    // Added for demo
    printPrograms(input, rewrittenProgram)
    rewrittenProgram
  }

  private[this] def printPrograms(beforeInline: Program, afterInline: Program): Unit = {
    println(s"============ INPUT PROGRAM ============")
    println(s"$beforeInline")
    println(s"========== END INPUT PROGRAM ==========")
    println(s"========== REWRITTEN PROGRAM ==========")
    println(s"$afterInline")
    println(s"======== END REWRITTEN PROGRAM ========")
  }
}
