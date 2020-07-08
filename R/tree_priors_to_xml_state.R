#' Converts one or more tree priors to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
tree_priors_to_xml_state <- function(
  tree_priors = "deprecated"
) {
  stop(
    "'tree_priors_to_xml_state' is deprecated, as",
    "only one tree prior is supported. ",
    "Use the (singular) 'tree_prior_to_xml_state' instead"
  )
}
