#' Get the number of distributions a site model has
#' @param site_model a site_model,
#'   as created by \code{\link{create_site_model}}
#' @return the number of distributions a site model has
#' @author Richel J.C. Bilderbeek
#' @export
get_site_model_n_params <- function(
  site_model
) {
  if (!is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  if (is_gtr_site_model(site_model)) {
    # gamma 0 alpha, gamma 0 beta
    # gamma 1 alpha, gamma 1 beta
    # gamma 2 alpha, gamma 2 beta
    # gamma 3 alpha, gamma 3 beta
    # gamma 5 alpha, gamma 5 beta
    return(10)
  } else if (is_hky_site_model(site_model)) {
    return(
      get_distr_n_params(site_model$kappa_prior_distr)
    )
  } else if (is_jc69_site_model(site_model)) {
    return(0)
  } else {
    testit::assert(beautier::is_tn93_site_model(site_model))
    return(
      get_distr_n_params(site_model$kappa_1_prior_distr) +
      get_distr_n_params(site_model$kappa_2_prior_distr)
    )
  }
}
