#' Creates the operators section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators <- function(
  ids,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule"),
  fixed_crown_age
) {

  if (!is.character(ids)) {
    stop("ids must be a character vector")
  }
  if (!is_tree_prior(tree_priors)) {
    stop("tree_priors must be one or more tree priors")
  }
  if (!is.logical(fixed_crown_age)) {
    stop("fixed_crown_age must be TRUE or FALSE")
  }

  text <- NULL
  text <- c(text, create_beast2_input_operators_tree_priors_1(
    ids = ids, tree_priors = tree_priors, fixed_crown_age = fixed_crown_age))

  # There are three parts: rate, freq and gamma. Order differs
  gamma_shape_scaler <- beautier::create_beast2_input_operators_gamma_shape_scaler(ids = ids, site_models = site_models)
  frequencies_exchanger <- beautier::create_beast2_input_operators_frequencies_exchanger(ids = ids, site_models = site_models)
  rates <- beautier::create_beast2_input_operators_rates(ids = ids, site_models = site_models)
  gcc <- get_gamma_cat_count(get_gamma_site_model(site_models = site_models))
  prop_invariant <- get_prop_invariant(get_gamma_site_model(site_models = site_models))

  if (is_gtr_site_model(site_models)) {
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

  text <- c(text, create_beast2_input_operators_tree_priors_2(
    ids = ids, tree_priors = tree_priors))

  # Clock models
  text <- c(text, create_beast2_input_operators_clock_models(
    ids = ids, clock_models = clock_models))

  text
}


#' Creates the first tree_priors section in the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_tree_priors_1 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors = create_tree_prior(name = "yule"),
  fixed_crown_age
) {
  text <- NULL
  operator_id_pre <- beautier::get_operator_id_pre(tree_priors)

  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("    <operator ",
      "id=\"YuleBirthRateScaler.t:", ids, "\" spec=\"ScaleOperator\" ",
      "parameter=\"@birthRate.t:", ids, "\" scaleFactor=\"0.75\" ",
      "weight=\"3.0\"/>"))
    text <- c(text, "")
  }

  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator ",
      "id=\"", operator_id_pre, "TreeScaler.t:", ids, "\" ",
      "spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      ids, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator ",
      "id=\"", operator_id_pre, "TreeRootScaler.t:", ids, "\" ",
      "spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", ids, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  text <- c(text, paste0("    <operator ",
    "id=\"", operator_id_pre, "UniformOperator.t:", ids, "\" spec=\"Uniform\" ",
    "tree=\"@Tree.t:", ids, "\" weight=\"30.0\"/>"))
  text <- c(text, "")
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator ",
      "id=\"", operator_id_pre, "SubtreeSlide.t:", ids, "\" ",
      "spec=\"SubtreeSlide\" tree=\"@Tree.t:", ids, "\" weight=\"15.0\"/>"))
    text <- c(text, "")
  }

  text <- c(text, paste0("    <operator ",
    "id=\"", operator_id_pre, "Narrow.t:", ids, "\" spec=\"Exchange\" ",
    "tree=\"@Tree.t:", ids, "\" weight=\"15.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "Wide.t:", ids,
    "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", ids,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator ",
    "id=\"", operator_id_pre, "WilsonBalding.t:", ids,
    "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", ids,
    "\" weight=\"3.0\"/>"))

  text
}

#' Creates the second tree_priors section in the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_tree_priors_2 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors = create_tree_prior(name = "yule")
) {
  text <- NULL
  if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"BirthRateScaler.t:",
      ids, "\" spec=\"ScaleOperator\" parameter=\"@BDBirthRate.t:",
      ids, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"DeathRateScaler.t:",
      ids,
      "\" spec=\"ScaleOperator\" parameter=\"@BDDeathRate.t:",
      ids, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"PopSizeScaler.t:",
      ids, "\" spec=\"ScaleOperator\" parameter=\"@popSize.t:", ids,
      "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"popSizesScaler.t:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@bPopSizes.t:", ids, "\" ",
      "scaleFactor=\"0.75\" weight=\"15.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"groupSizesDelta.t:", ids, "\" ",
      "spec=\"DeltaExchangeOperator\" integer=\"true\" weight=\"6.0\">"))
    text <- c(text, paste0("        <intparameter ",
      "idref=\"bGroupSizes.t:", ids, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  }
  text
}

#' Creates the site_models section in the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models = create_site_model(name = "JC69")
) {
  text <- NULL

  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"KappaScaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"kappa1Scaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa1.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"kappa2Scaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa2.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
  } else if (is_gtr_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateACScaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateAC.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateAGScaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateAG.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateATScaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateAT.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateCGScaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateCG.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateGTScaler.s:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@rateGT.s:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>"))
  }
}

#' Creates the gammaShapeScaler of the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_gamma_shape_scaler <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models = create_site_model(name = "JC69")
) {
  text <- NULL
  if (get_gamma_cat_count(get_gamma_site_model(site_models)) > 1) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"gammaShapeScaler.s:", ids, "\" spec=\"ScaleOperator\" ",
      "parameter=\"@gammaShape.s:", ids, "\" scaleFactor=\"0.5\" ",
      "weight=\"0.1\"/>"))
  }
  text
}

#' Creates the FrequenciesExchanger of the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_frequencies_exchanger <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models = create_site_model(name = "JC69")
) {
  text <- NULL
  if (!is_jc69_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"FrequenciesExchanger.s:", ids, "\" spec=\"DeltaExchangeOperator\" ",
      "delta=\"0.01\" weight=\"0.1\">"))
    text <- c(text, paste0("        <parameter ",
      "idref=\"freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  }
  text
}

#' Creates the clock_models section in the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_operators_clock_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  clock_models = create_clock_model(name = "strict")
) {
  text <- NULL
  if (is_rln_clock_model(clock_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"ucldStdevScaler.c:", ids, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:", ids, "\" ",
      "scaleFactor=\"0.5\" weight=\"3.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"CategoriesRandomWalk.c:", ids, "\" spec=\"IntRandomWalkOperator\" ",
      "parameter=\"@rateCategories.c:", ids, "\" weight=\"10.0\" ",
      "windowSize=\"1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"CategoriesSwapOperator.c:", ids, "\" spec=\"SwapOperator\" ",
      "intparameter=\"@rateCategories.c:", ids, "\" weight=\"10.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator ",
      "id=\"CategoriesUniform.c:", ids, "\" spec=\"UniformOperator\" ",
      "parameter=\"@rateCategories.c:", ids, "\" weight=\"10.0\"/>"))
  }
  text
}
