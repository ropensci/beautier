#' Create all clock models' operators' XML text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_operators <- function(
  clock_models,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(beautier::are_clock_models(clock_models))

  text <- NULL
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    text <- c(
      text,
      clock_model_to_xml_operators(
        clock_model = clock_model,
        mrca_priors = mrca_priors,
        tipdates_filename = tipdates_filename
      )
    )
  }
  text
}
