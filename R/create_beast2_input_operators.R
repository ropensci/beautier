#' Creates the operators section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models = create_jc69_site_models(n = length(ids)),
  clock_models = create_strict_clock_models(n = length(ids)),
  tree_priors = create_yule_tree_priors(n = length(ids)),
  fixed_crown_age
) {

  if (is_tree_prior(tree_priors)) {
    tree_priors <- list(tree_priors)
  }
  testit::assert(beautier::are_ids(ids))
  testit::assert(is.logical(fixed_crown_age))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))

  text <- NULL
  n <- length(ids)
  for (i in seq(1, n)) {

    id <- ids[i]
    site_model <- site_models[[i]]
    tree_prior <- tree_priors[[i]]
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_tree_prior(tree_prior))
    testit::assert(beautier::is_clock_model(clock_model))

    if (i > 1) {
      text <- c(text, "")
      text <- c(text, paste0("    <operator ",
        "id=\"StrictClockRateScaler.c:", id, "\" ",
        "spec=\"ScaleOperator\" ",
        "parameter=\"@clockRate.c:", id, "\" ",
        "scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    }

    text <- c(text, create_beast2_input_operators_tree_priors_1(
      id = id, tree_prior = tree_prior, fixed_crown_age = fixed_crown_age))

    if (i > 1) {
      text <- c(text, "")
      text <- c(text, paste0("    <operator ",
        "id=\"strictClockUpDownOperator.c:", id, "\" ",
        "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"))
      text <- c(text, paste0("        <up idref=\"clockRate.c:", id, "\"/>"))
      text <- c(text, paste0("        <down idref=\"Tree.t:", id, "\"/>"))
      text <- c(text, paste0("    </operator>"))
    }

    text <- c(text, create_beast2_input_operators_tree_priors_2(
      id = id, tree_prior = tree_prior, fixed_crown_age = fixed_crown_age))

    # There are three parts: rate, freq and gamma. Order differs
    gamma_shape_scaler <- beautier::create_beast2_input_operators_gamma_shape_scaler(id = id, site_model = site_model) # nolint
    frequencies_exchanger <- beautier::create_beast2_input_operators_frequencies_exchanger(id = id, site_model = site_model) # nolint
    rates <- beautier::create_beast2_input_operators_rates(id = id, site_model = site_model) # nolint
    gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model = site_model)) # nolint
    prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model = site_model)) # nolint

    if (is_gtr_site_model(site_model)) {
      if (gcc == 0) {
        text <- c(text, rates)
        text <- c(text, frequencies_exchanger)
      } else if (gcc == 1) {
        text <- c(text, frequencies_exchanger)
        text <- c(text, rates)
      } else {
        if (prop_invariant == get_default_prop_invariant()) {
          text <- c(text, frequencies_exchanger)
          text <- c(text, rates)
          text <- c(text, gamma_shape_scaler)
        } else {
          text <- c(text, gamma_shape_scaler)
          text <- c(text, frequencies_exchanger)
          text <- c(text, rates)
        }
      }
    } else {
      text <- c(text, rates)
      text <- c(text, gamma_shape_scaler)
      text <- c(text, frequencies_exchanger)
    }

    text <- c(text, create_beast2_input_operators_tree_priors_3(
      id = id, tree_prior = tree_prior))

    # Clock models
    text <- c(text, create_beast2_input_operators_clock_model(
      id = id, clock_model = clock_model))
  }
  text
}


#' Creates the first tree_priors section in the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param tree_prior tree prior, as created by \code{\link{create_tree_prior}}
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_tree_priors_1 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  tree_prior = create_tree_prior(name = "yule"),
  fixed_crown_age
) {
  text <- NULL

  if (is_yule_tree_prior(tree_prior)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator ",
      "id=\"YuleBirthRateScaler.t:", id, "\" spec=\"ScaleOperator\" ",
      "parameter=\"@birthRate.t:", id, "\" scaleFactor=\"0.75\" ",
      "weight=\"3.0\"/>"))
  }
  text
}

#' Creates the second tree_priors section in the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param tree_prior tree prior, as created by \code{\link{create_tree_prior}}
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_tree_priors_2 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  tree_prior = create_yule_tree_prior(),
  fixed_crown_age
) {
  text <- NULL
  operator_id_pre <- beautier::get_operator_id_pre(tree_prior)

  if (fixed_crown_age == FALSE) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator ",
      "id=\"", operator_id_pre, "TreeScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      id, "\" weight=\"3.0\"/>"))
  }
  if (fixed_crown_age == FALSE) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator ",
      "id=\"", operator_id_pre, "TreeRootScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", id, "\" weight=\"3.0\"/>"))
  }
  text <- c(text, "")
  text <- c(text, paste0("    <operator ",
    "id=\"", operator_id_pre, "UniformOperator.t:", id, "\" spec=\"Uniform\" ",
    "tree=\"@Tree.t:", id, "\" weight=\"30.0\"/>"))
  if (fixed_crown_age == FALSE) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator ",
      "id=\"", operator_id_pre, "SubtreeSlide.t:", id, "\" ",
      "spec=\"SubtreeSlide\" tree=\"@Tree.t:", id, "\" weight=\"15.0\"/>"))
  }

  text <- c(text, "")
  text <- c(text, paste0("    <operator ",
    "id=\"", operator_id_pre, "Narrow.t:", id, "\" spec=\"Exchange\" ",
    "tree=\"@Tree.t:", id, "\" weight=\"15.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "Wide.t:", id,
    "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", id,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator ",
    "id=\"", operator_id_pre, "WilsonBalding.t:", id,
    "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", id,
    "\" weight=\"3.0\"/>"))

  text
}

#' Creates the third tree_priors section in the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param tree_prior tree prior, as created by \code{\link{create_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_tree_priors_3 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  tree_prior = create_tree_prior(name = "yule")
) {
  text <- NULL
  if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"BirthRateScaler.t:",
      id, "\" spec=\"ScaleOperator\" parameter=\"@BDBirthRate.t:",
      id, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"DeathRateScaler.t:",
      id,
      "\" spec=\"ScaleOperator\" parameter=\"@BDDeathRate.t:",
      id, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"PopSizeScaler.t:",
      id, "\" spec=\"ScaleOperator\" parameter=\"@popSize.t:", id,
      "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"popSizesScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@bPopSizes.t:", id, "\" ",
      "scaleFactor=\"0.75\" weight=\"15.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"groupSizesDelta.t:", id, "\" ",
      "spec=\"DeltaExchangeOperator\" integer=\"true\" weight=\"6.0\">"))
    text <- c(text, paste0("        <intparameter ",
      "idref=\"bGroupSizes.t:", id, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  }
  text
}

#' Creates the site_models section in the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @inheritParams create_beast2_input_operators
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model = create_site_model(name = "JC69")
) {
  text <- NULL

  if (is_hky_site_model(site_model)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"KappaScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"kappa1Scaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa1.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"kappa2Scaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa2.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateACScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateAC.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateAGScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateAG.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateATScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateAT.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateCGScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateCG.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateGTScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateGT.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
  }
}

#' Creates the gammaShapeScaler of the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @inheritParams create_beast2_input_operators
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_gamma_shape_scaler <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model = create_site_model(name = "JC69")
) {
  text <- NULL
  if (get_gamma_cat_count(get_gamma_site_model(site_model)) > 1) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"gammaShapeScaler.s:", id, "\" spec=\"ScaleOperator\" ",
      "parameter=\"@gammaShape.s:", id, "\" scaleFactor=\"0.5\" ",
      "weight=\"0.1\"/>"))
  }
  text
}

#' Creates the FrequenciesExchanger of the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_frequencies_exchanger <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model = create_site_model(name = "JC69")
) {
  text <- NULL
  if (!is_jc69_site_model(site_model)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"FrequenciesExchanger.s:", id, "\" spec=\"DeltaExchangeOperator\" ",
      "delta=\"0.01\" weight=\"0.1\">"))
    text <- c(text, paste0("        <parameter ",
      "idref=\"freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  }
  text
}

#' Creates the clock_models section in the operators section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_clock_model <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model = create_strict_clock_model()
) {
  testit::assert(beautier::is_clock_model(clock_model))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"ucldStdevScaler.c:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"3.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"CategoriesRandomWalk.c:", id, "\" spec=\"IntRandomWalkOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\" ",
      "windowSize=\"1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"CategoriesSwapOperator.c:", id, "\" spec=\"SwapOperator\" ",
      "intparameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"CategoriesUniform.c:", id, "\" spec=\"UniformOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>"))
  }
  text
}
