#' Rename the filenames in an inference model
#' @inheritParams default_params_doc
#' @export
rename_inference_model_filenames <- function(
  inference_model,
  rename_fun
) {
  beautier::check_inference_model(inference_model)
  beautier::check_rename_fun(rename_fun)

  # mcmc
  inference_model$mcmc <- beautier::rename_mcmc_filenames(
    inference_model$mcmc,
    rename_fun = rename_fun
  )

  # inference_model tipdates
  inference_model$tipdates_filename <- rename_fun(
    inference_model$tipdates_filename
  )

  inference_model
}
