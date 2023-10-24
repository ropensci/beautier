#' Internal function
#'
#' Creates the XML of a tree prior,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
tree_prior_to_xml_operators <- function(
  inference_model
) {
  # Don't be smart yet
  tree_prior <- inference_model$tree_prior
  fixed_crown_age <- FALSE

  check_false(fixed_crown_age)
  check_true(is_tree_prior(tree_prior))
  id <- tree_prior$id
  check_true(is_id(id))

  text <- NULL
  operator_id_pre <- get_operator_id_pre(tree_prior)

  if (is_bd_tree_prior(tree_prior)) {
    text <- c(
      text,
      paste0(
        "<operator id=\"BirthRateScaler.t:",
        id, "\" spec=\"ScaleOperator\" parameter=\"@BDBirthRate.t:",
        id, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"
      )
    )
    text <- c(
      text,
      paste0(
        "<operator id=\"DeathRateScaler.t:",
        id,
        "\" spec=\"ScaleOperator\" parameter=\"@BDDeathRate.t:",
        id, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"
      )
    )
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(
      text,
      paste0(
        "<operator id=\"PopSizeScaler.t:",
        id, "\" spec=\"ScaleOperator\" parameter=\"@popSize.t:", id,
        "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"
      )
    )
  } else if (is_cbs_tree_prior(tree_prior)) {

    pop_size_scaler_xml <- paste0(
      "<operator id=\"popSizesScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@bPopSizes.t:", id, "\" "
    )
    check_true(
      "pop_sizes_scaler_scale_factor" %in% names(tree_prior)
    )
    if (tree_prior$pop_sizes_scaler_scale_factor != "") {
      pop_size_scaler_xml <- paste0(
        pop_size_scaler_xml,
        "scaleFactor=\"", tree_prior$pop_sizes_scaler_scale_factor, "\" "
      )
    }
    pop_size_scaler_xml <- paste0(pop_size_scaler_xml, "weight=\"15.0\"/>")

    text <- c(
      text,
      pop_size_scaler_xml
    )
    text <- c(
      text,
      paste0(
        "<operator id=\"groupSizesDelta.t:", id, "\" ",
        "spec=\"DeltaExchangeOperator\" integer=\"true\" weight=\"6.0\">"
      )
    )
    text <- c(
      text,
      paste0(
        "    <intparameter ",
        "idref=\"bGroupSizes.t:", id, "\"/>"
      )
    )
    text <- c(text, paste0("</operator>"))
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(
      text,
      paste0(
        "<operator id=\"ePopSizeScaler.t:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@ePopSize.t:", id, "\" ",
        "scaleFactor=\"0.75\" weight=\"3.0\"/>"
      )
    )
    text <- c(
      text,
      paste0(
        "<operator ",
        "id=\"GrowthRateRandomWalk.t:", id, "\" ",
        "spec=\"RealRandomWalkOperator\" parameter=\"@growthRate.t:", id, "\" ",
        "weight=\"3.0\" windowSize=\"1.0\"/>"
      )
    )
  } else {
    # Will fail on unimplemented tree priors
    check_true(is_yule_tree_prior(tree_prior))
    text <- c(
      text,
      yule_tree_prior_to_xml_operators(inference_model)
    )
  }

  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"", operator_id_pre, "TreeScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      id, "\" weight=\"3.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"", operator_id_pre, "TreeRootScaler.t:", id, "\" ",
      "spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", id, "\" weight=\"3.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"", operator_id_pre, "UniformOperator.t:", id, "\" ",
      "spec=\"Uniform\" ",
      "tree=\"@Tree.t:", id, "\" weight=\"30.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"", operator_id_pre, "SubtreeSlide.t:", id, "\" ",
      "spec=\"SubtreeSlide\" tree=\"@Tree.t:", id, "\" weight=\"15.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"", operator_id_pre, "Narrow.t:", id, "\" spec=\"Exchange\" ",
      "tree=\"@Tree.t:", id, "\" weight=\"15.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator id=\"", operator_id_pre, "Wide.t:", id,
      "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", id,
      "\" weight=\"3.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"", operator_id_pre, "WilsonBalding.t:", id,
      "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", id,
      "\" weight=\"3.0\"/>"
    )
  )
  text
}
