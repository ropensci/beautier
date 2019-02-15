#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of distributions a site model has
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   # gamma site model, rates AC, AG, AT, CG and GT
#'   testit::assert(
#'     beautier:::get_site_model_n_distrs(create_gtr_site_model()) == 6
#'   )
#'
#'   # gamma site model, kappa
#'   testit::assert(
#'     beautier:::get_site_model_n_distrs(create_hky_site_model()) == 2
#'   )
#'
#'   # gamma site model
#'   testit::assert(
#'     beautier:::get_site_model_n_distrs(create_jc69_site_model()) == 1
#'   )
#'
#'   # gamma site model, kappa 1 and kappa 2
#'   testit::assert(
#'     beautier:::get_site_model_n_distrs(create_tn93_site_model()) == 3
#'   )
#' @noRd
get_site_model_n_distrs <- function(
  site_model
) {
  if (!is_site_model(site_model)) { # nolint beautier function
    stop("'site_model' must be a site model")
  }
  gamma_site_model <- site_model$gamma_site_model
  gamma_site_model_n_distrs <- get_gamma_site_model_n_distrs(gamma_site_model) # nolint beautier function
  if (is_gtr_site_model(site_model)) { # nolint beautier function
    return(5 + gamma_site_model_n_distrs)
  } else if (is_hky_site_model(site_model)) { # nolint beautier function
    return(1 + gamma_site_model_n_distrs)
  } else if (is_jc69_site_model(site_model)) { # nolint beautier function
    return(0 + gamma_site_model_n_distrs)
  } else {
    testit::assert(is_tn93_site_model(site_model)) # nolint beautier function
    return(2 + gamma_site_model_n_distrs)
  }
}
