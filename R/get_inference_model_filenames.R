#' Get the filenames stored in an inference model.
#'
#' If there is no name for a \code{tipdates} file specified (as done by
#' setting \code{inference_model$tipdates_filename} to \link{NA},
#' there will be one filename less returned
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' inference_model <- create_inference_model()
#' inference_model$mcmc$tracelog$filename <- "/home/john/trace.log"
#' inference_model$mcmc$screenlog$filename <- "/home/john/screen.log"
#' inference_model$mcmc$treelog$filename <- "/home/john/tree.log"
#' inference_model$tipdates_filename <- "/home/john/tipdate.csv"
#'
#' filenames <- get_inference_model_filenames(inference_model)
#'
#' expect_equal(length(filenames), 4)
#' expect_true("/home/john/trace.log" %in% filenames)
#' expect_true("/home/john/screen.log" %in% filenames)
#' expect_true("/home/john/tree.log" %in% filenames)
#' expect_true("/home/john/tipdate.csv" %in% filenames)
#'
#' #' Nope, no tip dates needed in my inference ...
#' inference_model$tipdates_filename <- NA
#' filenames <- get_inference_model_filenames(inference_model)
#'
#' #' ... so one less file gets created
#' expect_equal(length(filenames), 3)
#' @export
get_inference_model_filenames <- function(inference_model) {
  beautier::check_inference_model(inference_model)
  stats::na.omit(
    c(
      get_mcmc_filenames(inference_model$mcmc),
      inference_model$tipdates_filename
    )
  )
}
