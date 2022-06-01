#' Internal function
#'
#' Creates the clock model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso all clock models' tracelog section is created
#'   by \code{\link{clock_models_to_xml_tracelog}}
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
clock_model_to_xml_tracelog <- function(
  inference_model,
  clock_model = "deprecated",
  mrca_priors = "deprecated"
) {
  testthat::expect_equal(clock_model, "deprecated")
  testthat::expect_equal(mrca_priors, "deprecated")
  # Do not be smart yet
  clock_model <- inference_model$clock_model
  mrca_priors  <- list(inference_model$mrca_prior)

  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_rln_clock_model(inference_model$clock_model)) {
    if (beautier::has_mrca_prior_with_distr(inference_model) ||
        beautier::has_tip_dating(inference_model)
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
    testit::assert(beautier::is_strict_clock_model(clock_model))
  }
  testit::assert(is.null(text) || beautier::is_xml(text))
  text
}
