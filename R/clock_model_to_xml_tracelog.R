#' Creates the clock model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso all clock models' tracelog section is created
#'   by \code{\link{clock_models_to_xml_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richèl J.C. Bilderbeek
#' @noRd
clock_model_to_xml_tracelog <- function(
  clock_model,
  mrca_priors = NA
) {
  testit::assert(is_clock_model(clock_model)) # nolint beautier function
  id <- clock_model$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  if (is_rln_clock_model(clock_model)) { # nolint beautier function
    if (is_mrca_prior_with_distr(mrca_priors[[1]]) # nolint beautier function
    ) {
      text <- c(text, paste0("<log idref=\"ucldMean.c:", id, "\"/>")) # nolint this is no absolute path
    }
    text <- c(text, paste0("<log idref=\"ucldStdev.c:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>") # nolint this is no absolute path # nolint this is no absolute path
    )
  } else {
    # Will fail on unimplemented clock models
    testit::assert(is_strict_clock_model(clock_model)) # nolint beautier function
  }
  testit::assert(is.null(text) || is_xml(text)) # nolint beautier function
  text
}
