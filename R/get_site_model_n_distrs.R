#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of distributions a site model has
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # rates AC, AG, AT, CG and GT
#'   testit::assert(get_site_model_n_distrs(create_gtr_site_model()) == 5)
#'
#'   # kappa
#'   testit::assert(get_site_model_n_distrs(create_hky_site_model()) == 1)
#'
#'   # no distributions
#'   testit::assert(get_site_model_n_distrs(create_jc69_site_model()) == 0)
#'
#'   # kappa 1 and kappa 2
#'   testit::assert(get_site_model_n_distrs(create_tn93_site_model()) == 2)
#' @export
get_site_model_n_distrs <- function(
  site_model
) {
  if (!beautier::is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  if (is_gtr_site_model(site_model)) {
    return(5)
  } else if (is_hky_site_model(site_model)) {
    return(1)
  } else if (is_jc69_site_model(site_model)) {
    return(0)
  } else {
    testit::assert(beautier::is_tn93_site_model(site_model))
    return(2)
  }
}
