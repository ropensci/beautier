#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of distributions a site model has
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # 5: rates AC, AG, AT, CG and GT
#' get_site_model_n_distrs(create_gtr_site_model())
#'
#' # 1: kappa
#' get_site_model_n_distrs(create_hky_site_model())
#'
#' # 0: npne
#' get_site_model_n_distrs(create_jc69_site_model())
#'
#' # 2: kappa 1 and kappa 2
#' get_site_model_n_distrs(create_tn93_site_model())
#' @export
get_site_model_n_distrs <- function(
  site_model
) {
  if (!beautier::is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  gamma_site_model <- site_model$gamma_site_model
  gamma_site_model_n_distrs <- beautier::get_gamma_site_model_n_distrs(
    gamma_site_model
  )
  if (beautier::is_gtr_site_model(site_model)) {
    return(5 + gamma_site_model_n_distrs)
  } else if (beautier::is_hky_site_model(site_model)) {
    return(1 + gamma_site_model_n_distrs)
  } else if (beautier::is_jc69_site_model(site_model)) {
    return(0 + gamma_site_model_n_distrs)
  } else {
    testit::assert(beautier::is_tn93_site_model(site_model))
    return(2 + gamma_site_model_n_distrs)
  }
}
