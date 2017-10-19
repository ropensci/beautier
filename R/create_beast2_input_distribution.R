#' Creates the distribution section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using 'get_file_base_sans_ext')
#' @param site_models one or more site models,
#'   as returned by 'create_site_model'
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_distribution <- function(
  ids,
  site_models = create_site_model(name = "JC69"),
  tree_priors = create_tree_prior(name = "yule")
) {
  text <- NULL
  text <- c(text,
    "    <distribution id=\"posterior\" spec=\"util.CompoundDistribution\">")
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("            <distribution id=\"YuleModel.t:", ids, "\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:", ids, "\" tree=\"@Tree.t:", ids, "\"/>"))
    text <- c(text, paste0("            <prior id=\"YuleBirthRatePrior.t:", ids, "\" name=\"distribution\" x=\"@birthRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("            <distribution id=\"BirthDeath.t:", ids, "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" birthDiffRate=\"@BDBirthRate.t:", ids, "\" relativeDeathRate=\"@BDDeathRate.t:", ids, "\" tree=\"@Tree.t:", ids, "\"/>"))
    text <- c(text, paste0("            <prior id=\"BirthRatePrior.t:", ids, "\" name=\"distribution\" x=\"@BDBirthRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"1000.0\"/>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"DeathRatePrior.t:", ids, "\" name=\"distribution\" x=\"@BDDeathRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.4\" name=\"distr\"/>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("            <distribution id=\"CoalescentConstant.t:", ids, "\" spec=\"Coalescent\">"))
    text <- c(text, paste0("                <populationModel id=\"ConstantPopulation.t:", ids, "\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:", ids, "\"/>"))
    text <- c(text, paste0(
      "                <treeIntervals id=\"TreeIntervals.t:",
      ids, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
      ids, "\"/>"))
    text <- c(text, "            </distribution>")
    text <- c(text, paste0(
      "            <prior id=\"PopSizePrior.t:", ids,
      "\" name=\"distribution\" x=\"@popSize.t:",
      ids, "\">"))
    text <- c(text, "                <OneOnX id=\"OneOnX.1\" name=\"distr\"/>")
    text <- c(text, "            </prior>")
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, paste0("            <distribution id=\"BayesianSkyline.t:", ids, "\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:", ids, "\" popSizes=\"@bPopSizes.t:", ids, "\">"))
    text <- c(text, paste0("                <treeIntervals id=\"BSPTreeIntervals.t:", ids, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:", ids, "\"/>"))
    text <- c(text, paste0("            </distribution>"))
    text <- c(text, paste0("            <distribution id=\"MarkovChainedPopSizes.t:", ids, "\" spec=\"beast.math.distributions.MarkovChainDistribution\" jeffreys=\"true\" parameter=\"@bPopSizes.t:", ids, "\"/>"))
  }

  # Site models
  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("            <prior id=\"KappaPrior.s:", ids, "\" name=\"distribution\" x=\"@kappa.s:", ids, "\">"))
    text <- c(text, paste0("                <LogNormal id=\"LogNormalDistributionModel.0\" name=\"distr\">"))
    text <- c(text, paste0("                    <parameter id=\"RealParameter.1\" estimate=\"false\" name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0("            <prior id=\"kappa1Prior.s:", ids, "\" name=\"distribution\" x=\"@kappa1.s:", ids, "\">"))
    text <- c(text, paste0("                <LogNormal id=\"LogNormalDistributionModel.1\" name=\"distr\">"))
    text <- c(text, paste0("                    <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter id=\"RealParameter.4\" estimate=\"false\" name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"kappa2Prior.s:", ids, "\" name=\"distribution\" x=\"@kappa2.s:", ids, "\">"))
    text <- c(text, paste0("                <LogNormal id=\"LogNormalDistributionModel.2\" name=\"distr\">"))
    text <- c(text, paste0("                    <parameter id=\"RealParameter.5\" estimate=\"false\" name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter id=\"RealParameter.6\" estimate=\"false\" name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
  }

  text <- c(text, "        </distribution>")
  text <- c(
      text,
      paste0(
        "        <distribution id=\"likelihood\" ",
        "spec=\"util.CompoundDistribution\" useThreads=\"true\">"
      )
    )
  text <- c(text, paste0("            <distribution id=\"treeLikelihood.",
    ids, "\" spec=\"ThreadedTreeLikelihood\" data=\"@", ids,
    "\" tree=\"@Tree.t:", ids, "\">"))
  text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
    ids, "\" spec=\"SiteModel\">"))
  text <- c(text, paste0("                    <parameter id=\"mutationRate.s:",
    ids,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
  text <- c(text, paste0("                    <parameter id=\"gammaShape.s:",
    ids,
    "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
  text <- c(text, paste0(
    "                    <parameter id=\"proportionInvariant.s:",
    ids, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>"))

  # Site models
  if (is_jc69_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel id=\"JC69.s:", ids, "\" spec=\"JukesCantor\"/>"))
  } else if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel id=\"hky.s:", ids, "\" spec=\"HKY\" kappa=\"@kappa.s:", ids, "\">"))
    text <- c(text, paste0("                        <frequencies id=\"estimatedFreqs.s:", ids, "\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel id=\"tn93.s:", ids, "\" spec=\"TN93\" kappa1=\"@kappa1.s:", ids, "\" kappa2=\"@kappa2.s:", ids, "\">"))
    text <- c(text, paste0("                        <frequencies id=\"estimatedFreqs.s:", ids, "\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  }

  text <- c(text, "                </siteModel>")
  text <- c(text, paste0("                <branchRateModel id=\"StrictClock.c:",
    ids,
    "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
  text <- c(text, paste0("                    <parameter id=\"clockRate.c:",
    ids,
    "\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>"))
  text <- c(text, "                </branchRateModel>")
  text <- c(text, "            </distribution>")
  text <- c(text, "        </distribution>")
  text <- c(text, "    </distribution>")
  text
}
