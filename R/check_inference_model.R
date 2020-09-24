#' Check if the supplied object is a valid
#' Bayesian phylogenetic inference model.
#'
#' Calls \code{stop} if the supplied object is not a valid
#'   Bayesian phylogenetic inference model.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_inference_model} to create a valid Bayesian
#'   phylogenetic inference model
#' @examples
#' check_inference_model(create_inference_model())
#' @author Richèl J.C. Bilderbeek
#' @export
check_inference_model <- function(
  inference_model
) {
  argument_names <- c(
    "site_model", "clock_model", "tree_prior",
    "mrca_prior", "mcmc", "beauti_options", "tipdates_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(inference_model)) {
      stop(
        "'", arg_name, "' must be an element of an 'inference_model'. \n",
        "Tip: use 'create_inference_model'"
      )
    }
  }
  tryCatch(
    beautier::check_site_model(inference_model$site_model),
    error = function(e) {
      stop(
        "'site_model' must be a valid site model. \n",
        "Error: ", e$message, "\n",
        "Value: ", inference_model$site_model
      )
    }
  )

  tryCatch(
    beautier::check_clock_model(inference_model$clock_model),
    error = function(e) {
      stop(
        "'clock_model' must be a valid clock model. \n",
        "Error: ", e$message, "\n",
        "Value: ", inference_model$clock_model
      )
    }
  )
  beautier::check_tree_prior(inference_model$tree_prior)
  beautier::check_mcmc(inference_model$mcmc)
  tryCatch(
    beautier::check_mrca_prior(inference_model$mrca_prior),
    error = function(e) {
      stop(
        "'mrca_prior' must be a valid MRCA prior. \n",
        "Error: ", e$message, "\n",
        "Value: ", inference_model$mrca_prior
      )
    }
  )
  tryCatch(
    beautier::check_beauti_options(inference_model$beauti_options),
    error = function(e) {
      stop(
        "'beauti_options' must be a valid BEAUti options. \n",
        "Error: ", e$message, "\n",
        "Value: ", inference_model$beauti_options
      )
    }
  )
  if (length(inference_model$tipdates_filename) != 1) {
    stop("'tipdates_filename' must have one element")
  }
  if (!beautier::is_one_na(inference_model$tipdates_filename) &&
    !assertive::is_a_string(inference_model$tipdates_filename)
  ) {
    stop("'tipdates_filename' must be one NA or one string")
  }
}
