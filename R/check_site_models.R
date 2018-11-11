#' Check if the site models are valid.
#' Calls \code{stop} if the site models are invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richel J.C. Bilderbeek
#' @noRd
check_site_models <- function(site_models) {

  if (is_site_model(site_models)) { # nolint internal function
    site_models <- list(site_models)
  }
  if (!are_site_models(site_models)) { # nolint internal function
    stop(
      "'site_models' must be a valid site model, ",
      "or a list of valid site models, ",
      "as returned by 'create_site_model'"
    )
  }

}
