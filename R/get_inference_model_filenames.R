#' Get the filenames stored in an inference model.
#'
#' If there is no name for a \code{tipdates} file specified (as done by
#' setting \code{inference_model$tipdates_filename} to \link{NA},
#' there will be one filename less returned
#' @inheritParams default_params_doc
#' @examples
#' check_empty_beautier_folder()
#'
#' inference_model <- create_inference_model()
#' filenames <- get_inference_model_filenames(inference_model)
#'
#' check_empty_beautier_folder()
#' @export
get_inference_model_filenames <- function(inference_model) {
  beautier::check_inference_model(inference_model)
  stats::na.omit(
    c(
      beautier::get_mcmc_filenames(inference_model$mcmc),
      inference_model$tipdates_filename
    )
  )
}
