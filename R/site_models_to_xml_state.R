#' Deprecated function
#'
#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
site_models_to_xml_state <- function(
  site_models = "deprecated"
) {
  stop(
    "Deprecated since beautier v2.5.",
    "See v2.4 for the code"
  )

}
