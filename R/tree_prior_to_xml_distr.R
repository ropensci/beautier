#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams create_beast2_input_distr
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
tree_prior_to_xml_distr <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_prior
) {
  text <- NULL
  id <- tree_prior$id
  if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, paste0("            <distribution id=\"BirthDeath.t:", id,
      "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" ",
      "birthDiffRate=\"@BDBirthRate.t:", id, "\" ",
      "relativeDeathRate=\"@BDDeathRate.t:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>"))
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, paste0("            <distribution ",
      "id=\"BayesianSkyline.t:",
      id, "\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:", id,
      "\" popSizes=\"@bPopSizes.t:", id, "\">"))
    text <- c(text, paste0("                ",
      "<treeIntervals id=\"BSPTreeIntervals.t:", id, "\" ",
      "spec=\"TreeIntervals\" tree=\"@Tree.t:", id, "\"/>"))
    text <- c(text, paste0("            </distribution>"))
    text <- c(text, paste0("            ",
      "<distribution id=\"MarkovChainedPopSizes.t:", id,
      "\" spec=\"beast.math.distributions.MarkovChainDistribution\" ",
      "jeffreys=\"true\" parameter=\"@bPopSizes.t:", id, "\"/>"))
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, paste0("            ",
      "<distribution id=\"CoalescentConstant.t:", id,
      "\" spec=\"Coalescent\">"))
    text <- c(text, paste0("                ",
      "<populationModel id=\"ConstantPopulation.t:", id,
      "\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:", id, "\"/>"))
    text <- c(text, paste0(
      "                <treeIntervals id=\"TreeIntervals.t:",
      id, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
      id, "\"/>"))
    text <- c(text, "            </distribution>")
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, paste0("            <distribution ",
      "id=\"CoalescentExponential.t:", id, "\" spec=\"Coalescent\">"))
    text <- c(text, paste0("                <populationModel ",
      "id=\"ExponentialGrowth.t:", id, "\" spec=\"ExponentialGrowth\" ",
      "growthRate=\"@growthRate.t:", id, "\" ",
      "popSize=\"@ePopSize.t:", id, "\"/>"))
    text <- c(text, paste0("                <treeIntervals ",
      "id=\"TreeIntervals.t:", id, "\" spec=\"TreeIntervals\" ",
      "tree=\"@Tree.t:", id, "\"/>"))
    text <- c(text, paste0("            </distribution>"))
  } else {
    testit::assert(is_yule_tree_prior(tree_prior))
    text <- c(text, paste0("            <distribution id=\"YuleModel.t:", id,
      "\" spec=\"beast.evolution.speciation.YuleModel\" ",
      "birthDiffRate=\"@birthRate.t:", id, "\" tree=\"@Tree.t:", id, "\"/>"))
  }

  text
}
