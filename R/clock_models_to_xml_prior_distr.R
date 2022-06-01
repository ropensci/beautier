#' Deprecated function
#'
#' Internal function to represent the clock models as XML
#' @examples
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_prior_distr <- function( # nolint indeed long function name
  clock_models = "deprecated",
  mrca_priors = "deprecated",
  tipdates_filename = "deprecated"
) {
  stop("deprecated, use beautier::clock_model_to_xml_prior_distr")
}
