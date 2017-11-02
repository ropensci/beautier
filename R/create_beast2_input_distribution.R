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
  site_models = create_jc69_site_models(length(ids)),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule")
) {
  if (length(ids) != length(site_models)) {
    stop("Must supply as much IDs as site_model objects")
  }

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

  n <- length(ids)
  for (i in seq(1, n)) {
    id <- ids[i]
    site_model <- site_models[[i]]
    site_models_text <- create_beast2_input_distribution_site_models(id = id, site_model = site_model) # nolint
    gamma_site_models_text <- create_beast2_input_distribution_gamma_site_models(id = id, site_model = site_model) # nolint
    prop_invariant <- get_prop_invariant(get_gamma_site_model(site_model)) # nolint
    if (prop_invariant == get_default_prop_invariant()) {
      text <- c(text, site_models_text)
      text <- c(text, gamma_site_models_text)
    } else {
      text <- c(text, gamma_site_models_text)
      text <- c(text, site_models_text)
    }

    text <- c(text,
      create_beast2_input_distribution_clock_models(
        ids = ids,
        clock_models = clock_models
      )
    )
  }

  text <- c(text, "        </distribution>")
  text <- c(
      text,
      paste0(
        "        <distribution id=\"likelihood\" ",
        "spec=\"util.CompoundDistribution\" useThreads=\"true\">"
      )
    )

  n <- length(ids)
  for (i in seq(1, n)) {
    id <- ids[i]
    site_model <- site_models[[i]]

    text <- c(text, paste0("            <distribution id=\"treeLikelihood.",
      id, "\" spec=\"ThreadedTreeLikelihood\" data=\"@", id,
      "\" tree=\"@Tree.t:", id, "\">"))
    # gamma category count
    gamma_category_count <- beautier::get_gamma_cat_count(
      beautier::get_gamma_site_model(site_model))
    if (gamma_category_count == 0) {
      text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\">")
      )
    } else if (gamma_category_count == 1) {
      text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
        "\">")
      )
    } else {
      text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
        "\" shape=\"@gammaShape.s:", id, "\">")
      )
    }


    text <- c(text, paste0("                    <parameter ",
      "id=\"mutationRate.s:", id,
      "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
    if (gamma_category_count < 2) {
      text <- c(text, paste0("                    <parameter ",
        "id=\"gammaShape.s:", id,
        "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
    }

    # proportionInvariant
    text <- c(text, paste0(
      "                    <parameter id=\"proportionInvariant.s:",
      id, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"proportionInvariant\" upper=\"1.0\">",
      beautier::get_prop_invariant(
        beautier::get_gamma_site_model(site_model)
      ),
      "</parameter>"))

    text <- c(text,
      create_beast2_input_distribution_subst_model(
        id = id,
        site_model = site_model
      )
    )

    text <- c(text, "                </siteModel>")

    # Clock models
    if (is_strict_clock_model(clock_models)) {
      text <- c(text, paste0("                <branchRateModel ",
        "id=\"StrictClock.c:", id, "\" ",
        "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
      text <- c(text, paste0("                    <parameter id=\"clockRate.c:",
        id, "\" estimate=\"false\" name=\"clock.rate\">",
        get_clock_model_rate(clock_models),
        "</parameter>"))
      text <- c(text, "                </branchRateModel>")
    } else if (is_rln_clock_model(clock_models)) {
      text <- c(text, paste0("                <branchRateModel ",
        "id=\"RelaxedClock.c:", id, "\" ",
        "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
        "rateCategories=\"@rateCategories.c:", id, "\" ",
        "tree=\"@Tree.t:", id, "\">"))
      text <- c(text, paste0("                    <LogNormal ",
        "id=\"LogNormalDistributionModel.c:", id, "\" ",
        "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
      text <- c(text, paste0("                        <parameter ",
        "id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" ",
        "upper=\"1.0\">1.0</parameter>"))
      text <- c(text, paste0("                    </LogNormal>"))
      text <- c(text, paste0("                    <parameter ",
        "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
        "name=\"clock.rate\">1.0</parameter>"))
      text <- c(text, paste0("                </branchRateModel>"))
    }

    text <- c(text, "            </distribution>")
  }
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
  tree_priors = rep(create_tree_prior(name = "yule"), length(ids))
) {
  if (length(ids) != length(tree_priors)) {
    stop("Must supply as much IDs as tree priors")
  }
  text <- NULL
  n <- length(ids)
  for (i in seq(n)) {
    id <- ids[i]
    tree_prior <- tree_priors[i]
    if (is_yule_tree_prior(tree_prior)) {
      text <- c(text, paste0("            <distribution id=\"YuleModel.t:", id,
        "\" spec=\"beast.evolution.speciation.YuleModel\" ",
        "birthDiffRate=\"@birthRate.t:", id, "\" tree=\"@Tree.t:", id, "\"/>"))

      if (i == 2 && n > 1) {
        text <- c(text, paste0("            <prior ",
          "id=\"ClockPrior.c:", id, "\" name=\"distribution\" ",
          "x=\"@clockRate.c:", id, "\">"))
        text <- c(text, paste0("                <Uniform id=\"Uniform.3\" ",
          "name=\"distr\" upper=\"Infinity\"/>"))
        text <- c(text, paste0("            </prior>"))
      }

      uniform_id <- ifelse(i == 1, 1, 4)
      text <- c(text, paste0("            <prior id=\"YuleBirthRatePrior.t:",
        id, "\" name=\"distribution\" x=\"@birthRate.t:", id, "\">"))
      text <- c(text, paste0("                <Uniform ",
        "id=\"Uniform.", uniform_id, "\" ",
        "name=\"distr\" upper=\"Infinity\"/>"))
      text <- c(text, paste0("            </prior>"))
    } else if (is_bd_tree_prior(tree_prior)) {
      text <- c(text, paste0("            <distribution id=\"BirthDeath.t:", id,
        "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" ",
        "birthDiffRate=\"@BDBirthRate.t:", id, "\" ",
        "relativeDeathRate=\"@BDDeathRate.t:", id, "\" ",
        "tree=\"@Tree.t:", id, "\"/>"))
      text <- c(text, paste0("            <prior id=\"BirthRatePrior.t:", id,
        "\" name=\"distribution\" x=\"@BDBirthRate.t:", id, "\">"))
      text <- c(text, paste0("                <Uniform id=\"Uniform.3\" ",
        "name=\"distr\" upper=\"1000.0\"/>"))
      text <- c(text, paste0("            </prior>"))
      text <- c(text, paste0("            <prior id=\"DeathRatePrior.t:", id,
        "\" name=\"distribution\" x=\"@BDDeathRate.t:", id, "\">"))
      text <- c(text, paste0("                <Uniform id=\"Uniform.4\" ",
        "name=\"distr\"/>"))
      text <- c(text, paste0("            </prior>"))
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
      text <- c(text, paste0(
        "            <prior id=\"PopSizePrior.t:", id,
        "\" name=\"distribution\" x=\"@popSize.t:",
        id, "\">"))
      text <- c(text, paste0("                <OneOnX id=\"OneOnX.1\" ",
        "name=\"distr\"/>"))
      text <- c(text, "            </prior>")
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
    }
  }
  text
}


#' Creates the first site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @param id the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_site_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("            <prior id=\"KappaPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@kappa.s:", id, "\">"))
    text <- c(text, paste0("                <LogNormal ",
      "id=\"LogNormalDistributionModel.0\" name=\"distr\">"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.1\" estimate=\"false\" name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("                    <parameter ",
      "id=\"RealParameter.2\" estimate=\"false\" name=\"S\">1.25</parameter>"))
    text <- c(text, paste0("                </LogNormal>"))
    text <- c(text, paste0("            </prior>"))
  } else if (is_tn93_site_model(site_model)) {
    distribution_ids <- NULL
    param_ids <- NULL
    if (get_gamma_cat_count(get_gamma_site_model(site_model)) == 0) {
      distribution_ids <- seq(1, 2)
      param_ids <- seq(3, 6)
    } else {
      distribution_ids <- seq(0, 1)
      param_ids <- seq(1, 4)
    }
    text <- c(text, paste0("            <prior id=\"kappa1Prior.s:", id, "\" ",
      "name=\"distribution\" x=\"@kappa1.s:", id, "\">"))
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
    text <- c(text, paste0("            <prior id=\"kappa2Prior.s:", id, "\" ",
      "name=\"distribution\" x=\"@kappa2.s:", id, "\">"))
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
  } else if (is_gtr_site_model(site_model)) {
    first_param_id <- ifelse(
      get_gamma_cat_count(get_gamma_site_model(site_model)) == 0, 7, 1)

    param_ids <- c(
      seq(first_param_id, first_param_id + 7),
      seq(first_param_id + 10, first_param_id + 11)
    )

    text <- c(text, paste0("            <prior ",
      "id=\"RateACPrior.s:", id, "\" name=\"distribution\" ",
      "x=\"@rateAC.s:", id, "\">"))
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
    text <- c(text, paste0("            <prior id=\"RateAGPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateAG.s:", id, "\">"))
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
    text <- c(text, paste0("            <prior id=\"RateATPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateAT.s:", id, "\">"))
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
    text <- c(text, paste0("            <prior id=\"RateCGPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateCG.s:", id, "\">"))
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
    text <- c(text, paste0("            <prior id=\"RateGTPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateGT.s:", id, "\">"))
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
#' @param id alignment ID
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_gamma_site_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  gamma_site_model <- beautier::get_gamma_site_model(
    site_model = site_model)
  if (get_gamma_cat_count(gamma_site_model) >= 2) {
    text <- c(text, paste0("            <prior ",
      "id=\"GammaShapePrior.s:", id, "\" name=\"distribution\" ",
      "x=\"@gammaShape.s:", id, "\">"))
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
#' @param ids alignment ID
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
#' @param id alignment ID
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_distribution_subst_model <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (beautier::is_jc69_site_model(site_model)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"JC69.s:", id, "\" spec=\"JukesCantor\"/>"))
  } else if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"hky.s:", id, "\" spec=\"HKY\" kappa=\"@kappa.s:", id, "\">"))
    text <- c(text, paste0("                        <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"tn93.s:", id, "\" spec=\"TN93\" kappa1=\"@kappa1.s:", id, "\" ",
      "kappa2=\"@kappa2.s:", id, "\">"))
    text <- c(text, paste0("                        <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("                    <substModel ",
      "id=\"gtr.s:", id, "\" spec=\"GTR\" rateAC=\"@rateAC.s:", id, "\" ",
      "rateAG=\"@rateAG.s:", id, "\" rateAT=\"@rateAT.s:", id, "\" ",
      "rateCG=\"@rateCG.s:", id, "\" rateGT=\"@rateGT.s:", id, "\">"))
    text <- c(text, paste0("                        <parameter ",
      "id=\"rateCT.s:", id, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"rateCT\">1.0</parameter>"))
    text <- c(text, paste0("                        <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("                    </substModel>"))
  }
  text
}
