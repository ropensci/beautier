#' Creates the distribution section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule")
) {
  text <- NULL
  text <- c(text,
    "    <distribution id=\"posterior\" spec=\"util.CompoundDistribution\">")
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  text <- c(text,
    create_beast2_input_distribution_distribution(
      ids = ids,
      tree_priors = tree_priors
    )
  )

  text <- c(text,
    create_beast2_input_distribution_site_models(
      ids = ids,
      site_models = site_models
    )
  )

  text <- c(text,
    create_beast2_input_distribution_gamma_site_models(
      ids = ids,
      site_models = site_models
    )
  )

  text <- c(text,
    create_beast2_input_distribution_clock_models(
      ids = ids,
      clock_models = clock_models
    )
  )

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
  # gamma category count
  gamma_category_count <- beautier::get_gamma_cat_count(
    beautier::get_gamma_site_model(site_models))
  if (gamma_category_count == 0) {
    text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
      ids, "\" spec=\"SiteModel\">")
    )
  } else if (gamma_category_count == 1) {
    text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
      ids, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
      "\">")
    )
  } else {
    text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
      ids, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
      "\" shape=\"@gammaShape.s:", ids, "\">")
    )
  }


  text <- c(text, paste0("                    <parameter id=\"mutationRate.s:",
    ids,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
  if (gamma_category_count < 2) {
    text <- c(text, paste0("                    <parameter id=\"gammaShape.s:",
      ids,
      "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
  }

  # proportionInvariant
  text <- c(text, paste0(
    "                    <parameter id=\"proportionInvariant.s:",
    ids, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">",
    beautier::get_prop_invariant(
      beautier::get_gamma_site_model(site_models)
    ),
    "</parameter>"))

  text <- c(text,
    create_beast2_input_distribution_subst_model(
      ids = ids,
      site_models = site_models
    )
  )

  text <- c(text, "                </siteModel>")

  # Clock models
  if (is_strict_clock_model(clock_models)) {
    text <- c(text, paste0("                <branchRateModel ",
      "id=\"StrictClock.c:", ids, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
    text <- c(text, paste0("                    <parameter id=\"clockRate.c:",
      ids, "\" estimate=\"false\" name=\"clock.rate\">",
      get_clock_model_rate(clock_models),
      "</parameter>"))
    text <- c(text, "                </branchRateModel>")
  } else if (is_rln_clock_model(clock_models)) {
    text <- c(text, paste0("                <branchRateModel ",
      "id=\"RelaxedClock.c:", ids, "\" ",
      "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
      "rateCategories=\"@rateCategories.c:", ids, "\" ",
      "tree=\"@Tree.t:", ids, "\">"))
    text <- c(text, paste0("                    <LogNormal ",
      "id=\"LogNormalDistributionModel.c:", ids, "\" ",
      "S=\"@ucldStdev.c:", ids, "\" meanInRealSpace=\"true\" name=\"distr\">"))
    text <- c(text, paste0("                        <parameter ",
      "id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" ",
      "upper=\"1.0\">1.0</parameter>"))
    text <- c(text, paste0("                    </LogNormal>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"ucldMean.c:", ids, "\" estimate=\"false\" ",
      "name=\"clock.rate\">1.0</parameter>"))
    text <- c(text, paste0("                </branchRateModel>"))
  }

  text <- c(text, "            </distribution>")
  text <- c(text, "        </distribution>")
  text <- c(text, "    </distribution>")
  text
}

#' Creates the distribution section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distribution
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_distribution <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors = create_tree_prior(name = "yule")
) {
  text <- NULL
  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("            <distribution id=\"YuleModel.t:", ids,
      "\" spec=\"beast.evolution.speciation.YuleModel\" ",
      "birthDiffRate=\"@birthRate.t:", ids, "\" tree=\"@Tree.t:", ids, "\"/>"))
    text <- c(text, paste0("            <prior id=\"YuleBirthRatePrior.t:",
      ids, "\" name=\"distribution\" x=\"@birthRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.1\" ",
      "name=\"distr\" upper=\"Infinity\"/>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("            <distribution id=\"BirthDeath.t:", ids,
      "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" ",
      "birthDiffRate=\"@BDBirthRate.t:", ids, "\" ",
      "relativeDeathRate=\"@BDDeathRate.t:", ids, "\" ",
      "tree=\"@Tree.t:", ids, "\"/>"))
    text <- c(text, paste0("            <prior id=\"BirthRatePrior.t:", ids,
      "\" name=\"distribution\" x=\"@BDBirthRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.3\" ",
      "name=\"distr\" upper=\"1000.0\"/>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"DeathRatePrior.t:", ids,
      "\" name=\"distribution\" x=\"@BDDeathRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.4\" ",
      "name=\"distr\"/>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("            ",
      "<distribution id=\"CoalescentConstant.t:", ids,
      "\" spec=\"Coalescent\">"))
    text <- c(text, paste0("                ",
      "<populationModel id=\"ConstantPopulation.t:", ids,
      "\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:", ids, "\"/>"))
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
    text <- c(text, paste0("            <distribution id=\"BayesianSkyline.t:",
      ids, "\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:", ids,
      "\" popSizes=\"@bPopSizes.t:", ids, "\">"))
    text <- c(text, paste0("                ",
      "<treeIntervals id=\"BSPTreeIntervals.t:", ids, "\" ",
      "spec=\"TreeIntervals\" tree=\"@Tree.t:", ids, "\"/>"))
    text <- c(text, paste0("            </distribution>"))
    text <- c(text, paste0("            ",
      "<distribution id=\"MarkovChainedPopSizes.t:", ids,
      "\" spec=\"beast.math.distributions.MarkovChainDistribution\" ",
      "jeffreys=\"true\" parameter=\"@bPopSizes.t:", ids, "\"/>"))
  }
  text
}


#' Creates the first site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distribution
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_site_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("            <prior id=\"KappaPrior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@kappa.s:", ids, "\">"))
    text <- c(text, paste0("                <LogNormal ",
      "id=\"LogNormalDistributionModel.0\" name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.1\" estimate=\"false\" name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.2\" estimate=\"false\" name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_tn93_site_model(site_models)) {
    distribution_ids <- NULL
    param_ids <- NULL
    if (get_gamma_cat_count(get_gamma_site_model(site_models)) == 0) {
      distribution_ids <- seq(1, 2)
      param_ids <- seq(3, 6)
    } else {
      distribution_ids <- seq(0, 1)
      param_ids <- seq(1, 4)
    }
    text <- c(text, paste0("            <prior id=\"kappa1Prior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@kappa1.s:", ids, "\">"))
    text <- c(text, paste0("                <LogNormal ",
      "id=\"LogNormalDistributionModel.", distribution_ids[1], "\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[1], "\" estimate=\"false\" ",
      "name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[2], "\" estimate=\"false\" ",
      "name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"kappa2Prior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@kappa2.s:", ids, "\">"))
    text <- c(text, paste0("                <LogNormal ",
      "id=\"LogNormalDistributionModel.", distribution_ids[2], "\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[3], "\" estimate=\"false\" ",
      "name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[4], "\" estimate=\"false\" ",
      "name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_gtr_site_model(site_models)) {
    first_param_id <- ifelse(
      get_gamma_cat_count(get_gamma_site_model(site_models)) == 0, 7, 1)

    param_ids <- c(
      seq(first_param_id, first_param_id + 7),
      seq(first_param_id + 10, first_param_id + 11)
    )

    text <- c(text, paste0("            <prior ",
      "id=\"RateACPrior.s:", ids, "\" name=\"distribution\" ",
      "x=\"@rateAC.s:", ids, "\">"))
    text <- c(text, paste0("                <Gamma id=\"Gamma.0\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[1], "\" ",
      "estimate=\"false\" ",
      "name=\"alpha\">0.05</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[2], "\" estimate=\"false\" ",
      "name=\"beta\">10.0</parameter>"))
    text <- c(text, paste0("                </Gamma>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"RateAGPrior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@rateAG.s:", ids, "\">"))
    text <- c(text, paste0("                <Gamma id=\"Gamma.1\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[3], "\" estimate=\"false\" ",
      "name=\"alpha\">0.05</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[4], "\" estimate=\"false\" ",
      "name=\"beta\">20.0</parameter>"))
    text <- c(text, paste0("                </Gamma>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"RateATPrior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@rateAT.s:", ids, "\">"))
    text <- c(text, paste0("                <Gamma id=\"Gamma.2\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[5], "\" estimate=\"false\" ",
      "name=\"alpha\">0.05</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[6], "\" estimate=\"false\" ",
      "name=\"beta\">10.0</parameter>"))
    text <- c(text, paste0("                </Gamma>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"RateCGPrior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@rateCG.s:", ids, "\">"))
    text <- c(text, paste0("                <Gamma id=\"Gamma.3\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[7], "\" estimate=\"false\" ",
      "name=\"alpha\">0.05</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[8], "\" estimate=\"false\" ",
      "name=\"beta\">10.0</parameter>"))
    text <- c(text, paste0("                </Gamma>"))
    text <- c(text, paste0("            </prior>"))
    text <- c(text, paste0("            <prior id=\"RateGTPrior.s:", ids, "\" ",
      "name=\"distribution\" x=\"@rateGT.s:", ids, "\">"))
    text <- c(text, paste0("                <Gamma id=\"Gamma.5\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[9], "\" estimate=\"false\" ",
      "name=\"alpha\">0.05</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.", param_ids[10], "\" estimate=\"false\" ",
      "name=\"beta\">10.0</parameter>"))
    text <- c(text, paste0("                </Gamma>"))
    text <- c(text, paste0("            </prior>"))
  }
  text
}

#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distribution
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_gamma_site_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  gamma_site_models <- beautier::get_gamma_site_model(
    site_models = site_models)
  if (get_gamma_cat_count(gamma_site_models) >= 2) {
    text <- c(text, paste0("            <prior ",
      "id=\"GammaShapePrior.s:", ids, "\" name=\"distribution\" ",
      "x=\"@gammaShape.s:", ids, "\">"))
    text <- c(text, paste0("                <Exponential id=\"Exponential.0\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.0\" estimate=\"false\" ",
      "name=\"mean\">1.0</parameter>"))
    text <- c(text, paste0("                </Exponential>"))
    text <- c(text, paste0("            </prior>"))
  }
  text
}

#' Creates the clock models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distribution
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_clock_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  clock_models
) {
  text <- NULL
  if (is_rln_clock_model(clock_models)) {
    text <- c(text, paste0("            <prior ",
      "id=\"ucldStdevPrior.c:", ids, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", ids, "\">"))
    text <- c(text, paste0("                <Gamma id=\"Gamma.0\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.2\" estimate=\"false\" ",
      "name=\"alpha\">0.5396</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.3\" estimate=\"false\" ",
      "name=\"beta\">0.3819</parameter>"))
    text <- c(text, paste0("                </Gamma>"))
    text <- c(text, paste0("            </prior>"))
  }
  text
}

#' Creates the substModel section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distribution
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_subst_model <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (beautier::is_jc69_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"JC69.s:", ids, "\" spec=\"JukesCantor\"/>"))
  } else if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"hky.s:", ids, "\" spec=\"HKY\" kappa=\"@kappa.s:", ids, "\">"))
    text <- c(text, paste0("                        <frequencies ",
      "id=\"estimatedFreqs.s:", ids, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"tn93.s:", ids, "\" spec=\"TN93\" kappa1=\"@kappa1.s:", ids, "\" ",
      "kappa2=\"@kappa2.s:", ids, "\">"))
    text <- c(text, paste0("                        <frequencies ",
      "id=\"estimatedFreqs.s:", ids, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  } else if (is_gtr_site_model(site_models)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"gtr.s:", ids, "\" spec=\"GTR\" rateAC=\"@rateAC.s:", ids, "\" ",
      "rateAG=\"@rateAG.s:", ids, "\" rateAT=\"@rateAT.s:", ids, "\" ",
      "rateCG=\"@rateCG.s:", ids, "\" rateGT=\"@rateGT.s:", ids, "\">"))
    text <- c(text, paste0("                        <parameter ",
      "id=\"rateCT.s:", ids, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"rateCT\">1.0</parameter>"))
    text <- c(text, paste0("                        <frequencies ",
      "id=\"estimatedFreqs.s:", ids, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  }
  text
}
