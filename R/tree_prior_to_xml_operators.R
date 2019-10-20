#' Creates the XML of a tree prior,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
tree_prior_to_xml_operators <- function(
  tree_prior,
  fixed_crown_age = FALSE
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  id <- tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  operator_id_pre <- get_operator_id_pre(tree_prior) # nolint beautier function

  if (beautier::is_bd_tree_prior(tree_prior)) {
    text <- c(text, paste0("<operator id=\"BirthRateScaler.t:",
      id, "\" spec=\"ScaleOperator\" parameter=\"@BDBirthRate.t:",
      id, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<operator id=\"DeathRateScaler.t:",
      id,
      "\" spec=\"ScaleOperator\" parameter=\"@BDDeathRate.t:",
      id, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>")) # nolint this is no absolute path
  } else if (beautier::is_ccp_tree_prior(tree_prior)) {
    text <- c(text, paste0("<operator id=\"PopSizeScaler.t:",
      id, "\" spec=\"ScaleOperator\" parameter=\"@popSize.t:", id,
      "\" scaleFactor=\"0.75\" weight=\"3.0\"/>")) # nolint this is no absolute path
  } else if (beautier::is_cbs_tree_prior(tree_prior)) {
    text <- c(text, paste0("<operator id=\"popSizesScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@bPopSizes.t:", id, "\" ",
      "scaleFactor=\"0.75\" weight=\"15.0\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<operator id=\"groupSizesDelta.t:", id, "\" ",
      "spec=\"DeltaExchangeOperator\" integer=\"true\" weight=\"6.0\">"))
    text <- c(text, paste0("    <intparameter ",
      "idref=\"bGroupSizes.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("</operator>"))
  } else if (beautier::is_cep_tree_prior(tree_prior)) {
    text <- c(text, paste0("<operator id=\"ePopSizeScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ePopSize.t:", id, "\" ",
      "scaleFactor=\"0.75\" weight=\"3.0\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<operator ",
      "id=\"GrowthRateRandomWalk.t:", id, "\" ",
      "spec=\"RealRandomWalkOperator\" parameter=\"@growthRate.t:", id, "\" ",
      "weight=\"3.0\" windowSize=\"1.0\"/>")) # nolint this is no absolute path
  } else {
    # Will fail on unimplemented tree priors
    testit::assert(beautier::is_yule_tree_prior(tree_prior))

    text <- c(text,
      paste0(
        "<operator id=\"YuleBirthRateScaler.t:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@birthRate.t:", id, "\" ",
        "scaleFactor=\"0.75\" weight=\"3.0\"/>" # nolint this is no absolute path
      )
    )
  }

  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("<operator ",
      "id=\"", operator_id_pre, "TreeScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      id, "\" weight=\"3.0\"/>")) # nolint this is no absolute path
  }
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("<operator ",
      "id=\"", operator_id_pre, "TreeRootScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", id, "\" weight=\"3.0\"/>")) # nolint this is no absolute path
  }
  text <- c(text, paste0("<operator ",
    "id=\"", operator_id_pre, "UniformOperator.t:", id, "\" spec=\"Uniform\" ",
    "tree=\"@Tree.t:", id, "\" weight=\"30.0\"/>")) # nolint this is no absolute path
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("<operator ",
      "id=\"", operator_id_pre, "SubtreeSlide.t:", id, "\" ",
      "spec=\"SubtreeSlide\" tree=\"@Tree.t:", id, "\" weight=\"15.0\"/>")) # nolint this is no absolute path
  }
  text <- c(text, paste0("<operator ",
    "id=\"", operator_id_pre, "Narrow.t:", id, "\" spec=\"Exchange\" ",
    "tree=\"@Tree.t:", id, "\" weight=\"15.0\"/>")) # nolint this is no absolute path
  text <- c(text, paste0("<operator id=\"", operator_id_pre, "Wide.t:", id,
    "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", id,
    "\" weight=\"3.0\"/>")) # nolint this is no absolute path
  text <- c(text, paste0("<operator ",
    "id=\"", operator_id_pre, "WilsonBalding.t:", id,
    "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", id,
    "\" weight=\"3.0\"/>")) # nolint this is no absolute path
  text
}
