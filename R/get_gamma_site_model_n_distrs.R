#' Get the number of distributions in a gamma site model
#' @inheritParams default_params_doc
#' @return the number of distributrion a gamma site model has
#' @examples
#'   gamma_site_model <- create_gamma_site_model(prop_invariant = 0.5)
#'
#'   gamma_site_model <- create_gamma_site_model()
#'   gamma_site_model_n_distrs <- get_gamma_site_model_n_distrs(
#'     gamma_site_model
#'    )
#'   testit::assert(gamma_site_model_n_distrs == 1)
#' @author Richel J.C. Bilderbeek
#' @export
get_gamma_site_model_n_distrs <- function(gamma_site_model) {
  testit::assert(is_gamma_site_model(gamma_site_model))
  1
}
