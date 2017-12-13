#' Creates the site models' XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_beast2_input_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
site_models_to_xml_tracelog <- function(
  site_models
) {
  text <- NULL
  for (site_model in site_models) {
    text <- c(text, site_model_to_xml_tracelog(site_model)) # nolint internal function
  }
  text
}
