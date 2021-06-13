#' Deprecated internal function
#'
#' Creates the clock models' XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_tracelog_xml}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_tracelog <- function(
  clock_models,
  mrca_priors = NA
) {
  stop(
    "'clock_models_to_xml_tracelog' is deprecated, ",
    "use 'clock_model_to_xml_tracelog' instead"
  )
}
