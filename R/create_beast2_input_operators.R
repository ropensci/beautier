#' Creates the operators section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using 'get_file_base_sans_ext')
#' @param site_models one or more site models,
#'   as returned by 'create_site_model'
#' @param tree_priors One or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @export
create_beast2_input_operators <- function(
  ids,
  site_models = create_site_model(name = "JC69"),
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

  operator_id_pre <- beastscriptr::get_operator_id_pre(tree_priors)

  text <- NULL

  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("    <operator id=\"YuleBirthRateScaler.t:", ids, "\" spec=\"ScaleOperator\" parameter=\"@birthRate.t:", ids, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }

  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"", operator_id_pre, "TreeScaler.t:", ids,
      "\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      ids, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"", operator_id_pre, "TreeRootScaler.t:",
      ids,
      "\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", ids, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "UniformOperator.t:", ids,
    "\" spec=\"Uniform\" tree=\"@Tree.t:", ids,
    "\" weight=\"30.0\"/>"))
  text <- c(text, "")
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"", operator_id_pre, "SubtreeSlide.t:", ids,
      "\" spec=\"SubtreeSlide\" tree=\"@Tree.t:", ids,
      "\" weight=\"15.0\"/>"))
    text <- c(text, "")
  }

  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "Narrow.t:", ids, "\" spec=\"Exchange\" tree=\"@Tree.t:", ids,
    "\" weight=\"15.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "Wide.t:", ids,
    "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", ids,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "WilsonBalding.t:", ids,
    "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", ids,
    "\" weight=\"3.0\"/>"))

  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"KappaScaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@kappa.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"FrequenciesExchanger.s:", ids, "\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">"))
    text <- c(text, paste0("        <parameter idref=\"freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"kappa1Scaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@kappa1.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"kappa2Scaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@kappa2.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"FrequenciesExchanger.s:", ids, "\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">"))
    text <- c(text, paste0("        <parameter idref=\"freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  } else if (is_gtr_site_model(site_models)) {
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateACScaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@rateAC.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateAGScaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@rateAG.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateATScaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@rateAT.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateCGScaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@rateCG.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"RateGTScaler.s:", ids, "\" spec=\"ScaleOperator\" parameter=\"@rateGT.s:", ids, "\" scaleFactor=\"0.5\" weight=\"0.1\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"FrequenciesExchanger.s:", ids, "\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">"))
    text <- c(text, paste0("        <parameter idref=\"freqParameter.s:", ids, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  }

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
    text <- c(text, paste0("    <operator id=\"popSizesScaler.t:", ids, "\" spec=\"ScaleOperator\" parameter=\"@bPopSizes.t:", ids, "\" scaleFactor=\"0.75\" weight=\"15.0\"/>"))
    text <- c(text, paste0(""))
    text <- c(text, paste0("    <operator id=\"groupSizesDelta.t:", ids, "\" spec=\"DeltaExchangeOperator\" integer=\"true\" weight=\"6.0\">"))
    text <- c(text, paste0("        <intparameter idref=\"bGroupSizes.t:", ids, "\"/>"))
    text <- c(text, paste0("    </operator>"))
  }
  text
}
