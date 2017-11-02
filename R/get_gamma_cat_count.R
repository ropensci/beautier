#' Extract the gamma category count from a gamma site model
#' @param gamma_site_model one or more gamma_site_models, as created
#'   by \code{\link{create_gamma_site_model}}
#' @return the gamma category count
#' @export
get_gamma_cat_count <- function(gamma_site_model) {

  if (!is_gamma_site_model(gamma_site_model)) {
    stop("site_models must be a gamma_site_model")
  }
  testit::assert("gamma_cat_count" %in% names(gamma_site_model))
  gamma_site_model$gamma_cat_count
}

#' Get the default gamma category count
#' @export
get_default_gamma_cat_count <- function() {
  return(0)
}
