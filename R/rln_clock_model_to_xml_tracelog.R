#' Internal function
#'
#' Creates the RLN clock model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @examples
#' check_empty_beautier_folder()
#'
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
rln_clock_model_to_xml_tracelog <- function( # nolint indeed a long internal function name
  inference_model
) {
  check_true(
    is_rln_clock_model(inference_model$clock_model)
  )
  id <- inference_model$clock_model$id
  check_true(is_id(id))

  text <- NULL
  if (has_mrca_prior_with_distr(inference_model) ||
      has_tip_dating(inference_model)
  ) {
    text <- c(text, paste0("<log idref=\"ucldMean.c:", id, "\"/>")) # nolint this is no absolute path
  }
  text <- c(text, paste0("<log idref=\"ucldStdev.c:", id, "\"/>")) # nolint this is no absolute path
  text <- c(
    text,
    paste0(
      "<log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>"
    )
  )
  text
}
