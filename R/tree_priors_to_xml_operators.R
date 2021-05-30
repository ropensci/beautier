#' Deprecated
#'
#' Creates the XML of a list of one or more tree priors,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the tree priors as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
tree_priors_to_xml_operators <- function(
  tree_priors = "deprecated",
  fixed_crown_ages = "deprecated"
) {
  stop("deprecated, use beautier::tree_prior_to_xml_operators")
}
