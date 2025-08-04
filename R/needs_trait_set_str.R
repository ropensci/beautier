#' Is it needed to add the tip dates as a string to the BEAST2 XML input file?
#'
#' The hypothesis is that this is needed when:
#' - the tree prior is a coalescent model
#' - all times are zero
#'
#' Here are the notes I concluded this from:
#'
#' Site model|Clock model|Tree prior|Needed|Other notes
#' ----------|-----------|----------|------|----------------
#' GTR       |RLN        |CBS       |Yes   |#108
#' HKY       |RLN        |CCP       |Yes   |#109
#' JC69      |RLN        |Yule      |No    |#99
#' JC69      |Strict     |Yule      |No    |tipdates_2_6.xml
#' JC69      |RLN        |Yule      |No    |tipdates_2_6.xml
#'
#' @inheritParams default_params_doc
#' @return TRUE if this is needed
#' @author Richèl J.C. Bilderbeek
#' @export
needs_trait_set_str <- function(inference_model) {
  check_inference_model(inference_model)

  # Unsure about this one
  # if (inference_model$beauti_options$beast2_version != "2.6") {
  #   return(FALSE)
  # }
  # if (is_one_na(inference_model$tipdates_filename)) {
  #   return(FALSE)
  # }
  t <- read_tipdates_file(inference_model$tipdates_filename)
  if (all(t$time == "0")) {
    return(FALSE)
  }
  # if (1 == 2) {
  #   tree_priors_that_need_it <- c(
  #     "coalescent_bayesian_skyline",
  #     "coalescent_constant_population",
  #     "coalescent_exp_population" # Untested, it just follows my hypothesis
  #   )
  #   inference_model$tree_prior$name %in% tree_priors_that_need_it
  # }
  TRUE
}
