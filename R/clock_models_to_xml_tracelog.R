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
#' @noRd
clock_models_to_xml_tracelog <- function(
  clock_models,
  mrca_priors = NA
) {
  testit::assert(beautier::are_clock_models(clock_models))

  text <- NULL
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))
    text <- c(
      text,
      clock_model_to_xml_tracelog( # nolint beautier function
        clock_model = clock_model,
        mrca_priors = mrca_priors
      )
    )
  }
  text
}
