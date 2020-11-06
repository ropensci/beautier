#' Deprecated
#'
#' Converts one or more MRCA priors to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_priors_to_xml_state <- function(
  inference_model = "deprecated",
  mrca_priors = "deprecated",
  has_non_strict_clock_model = "deprecated"
) {
  stop(
    "'mrca_priors_to_xml_state' is deprecated since beautier v2.5",
    "See v2.4 for (probably invalid, as badly tested) code"
  )
}
