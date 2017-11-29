#' Creates the operators section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models = create_jc69_site_models(ids = ids),
  clock_models = create_strict_clock_models(ids = ids),
  tree_priors = create_yule_tree_priors(ids = ids),
  fixed_crown_age
) {
  testit::assert(beautier::are_ids(ids))
  testit::assert(is.logical(fixed_crown_age))
  testit::assert(length(ids) >= length(site_models))
  testit::assert(length(ids) >= length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL

  # Tree priors is leading
  for (i in seq_along(tree_priors)) {
    tree_prior <- tree_priors[[i]]
    id <- tree_prior$id

    # Extra trees' clock models
    clock_model <- beautier::find_clock_model(
      clock_models = clock_models, id = id)
    if (!is.null(clock_model)) {
      if (i > 1) {
        text <- c(text, "")
        text <- c(text, paste0("    <operator ",
          "id=\"StrictClockRateScaler.c:", id, "\" ",
          "spec=\"ScaleOperator\" ",
          "parameter=\"@clockRate.c:", id, "\" ",
          "scaleFactor=\"0.75\" weight=\"3.0\"/>"))
      }
      if (is_yule_tree_prior(tree_prior)) {
        text <- c(text, "")
        text <- c(text, paste0("    <operator ",
          "id=\"YuleBirthRateScaler.t:", id, "\" spec=\"ScaleOperator\" ",
          "parameter=\"@birthRate.t:", id, "\" scaleFactor=\"0.75\" ",
          "weight=\"3.0\"/>"))
      }
      if (i > 1) {
        text <- c(text, "")
        text <- c(text, paste0("    <operator ",
          "id=\"strictClockUpDownOperator.c:", id, "\" ",
          "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"))
        text <- c(text, paste0("        <up idref=\"clockRate.c:", id, "\"/>"))
        text <- c(text, paste0("        <down idref=\"Tree.t:", id, "\"/>"))
        text <- c(text, paste0("    </operator>"))
      }
    }

    # Tree priors
    text <- c(text, create_beast2_input_operators_tree_priors(
      tree_prior = tree_prior, fixed_crown_age = fixed_crown_age))

    # Clock models with same ID
    clock_model <- beautier::find_clock_model(
      clock_models = clock_models, id = id)
    if (!is.null(clock_model)) {
      testit::assert(beautier::is_clock_model(clock_model))
      text <- c(text, create_beast2_input_operators_clock_model(
        id = id, clock_model = clock_model))
    }

    # Site models with same ID
    site_model <- beautier::find_site_model(site_models = site_models, id = id)
    if (!is.null(site_model)) {
      text <- c(text, site_model_to_xml_operators(site_model)) # nolint internal function
    }
  }

  text
}


#' Creates the first tree_priors section in the operators section
#' of a BEAST2 XML parameter file
#' @param tree_prior tree prior, as created by \code{\link{create_tree_prior}}
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators_tree_priors <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_prior,
  fixed_crown_age
) {
  id <- tree_prior$id
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
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"ePopSizeScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ePopSize.t:", id, "\" ",
      "scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    text <- c(text, "")
    text <- c(text, paste0("    <operator ",
      "id=\"GrowthRateRandomWalk.t:", id, "\" ",
      "spec=\"RealRandomWalkOperator\" parameter=\"@growthRate.t:", id, "\" ",
      "weight=\"3.0\" windowSize=\"1.0\"/>"))
  }
  text
}


#' Creates the site_models section in the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  id <- site_model$id
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
  text
}

#' Creates the gammaShapeScaler of the operators section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_operators
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators_gamma_shape_scaler <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  id <- site_model$id
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
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @inheritParams create_beast2_input_operators
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators_frequencies_exchanger <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  text <- NULL
  id <- site_model$id
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
