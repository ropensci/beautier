#' Rename the filenames in an inference model
#' @inheritParams default_params_doc
#' @return an inference model with the renamed filenames
#' @examples
#' library(testthat)
#'
#' inference_model <- create_inference_model()
#' inference_model$mcmc$tracelog$filename <- "trace.log"
#' inference_model$mcmc$screenlog$filename <- "screen.log"
#' inference_model$mcmc$treelog$filename <- "tree.log"
#' inference_model$tipdates_filename <- "tipdates.csv"
#'
#' # Nah, put the files in a folder
#' inference_model <- rename_inference_model_filenames(
#'   inference_model = inference_model,
#'   rename_fun = get_replace_dir_fun("/home/john")
#' )
#'
#' expect_equal(inference_model$mcmc$tracelog$filename, "/home/john/trace.log")
#' expect_equal(
#'   inference_model$mcmc$screenlog$filename, "/home/john/screen.log"
#' )
#' expect_equal(inference_model$mcmc$treelog$filename, "/home/john/tree.log")
#' expect_equal(inference_model$tipdates_filename, "/home/john/tipdates.csv")
#'
#' # Nah, put the files in anoth folder
#' inference_model <- rename_inference_model_filenames(
#'   inference_model = inference_model,
#'   rename_fun = get_replace_dir_fun("/home/doe")
#' )
#'
#' expect_equal(inference_model$mcmc$tracelog$filename, "/home/doe/trace.log")
#' expect_equal(inference_model$mcmc$screenlog$filename, "/home/doe/screen.log")
#' expect_equal(inference_model$mcmc$treelog$filename, "/home/doe/tree.log")
#' expect_equal(inference_model$tipdates_filename, "/home/doe/tipdates.csv")
#'
#' # Nah, store the files locally
#' inference_model <- rename_inference_model_filenames(
#'   inference_model = inference_model,
#'   rename_fun = get_remove_dir_fun()
#' )
#'
#' expect_equal(inference_model$mcmc$tracelog$filename, "trace.log")
#' expect_equal(inference_model$mcmc$screenlog$filename, "screen.log")
#' expect_equal(inference_model$mcmc$treelog$filename, "tree.log")
#' expect_equal(inference_model$tipdates_filename, "tipdates.csv")
#' @export
rename_inference_model_filenames <- function( # nolint long function name indeed
  inference_model,
  rename_fun
) {
  beautier::check_inference_model(inference_model)
  beautier::check_rename_fun(rename_fun)

  # MCMC
  inference_model$mcmc <- beautier::rename_mcmc_filenames(
    inference_model$mcmc,
    rename_fun = rename_fun
  )

  # tipdates
  inference_model$tipdates_filename <- rename_fun(
    inference_model$tipdates_filename
  )

  inference_model
}
