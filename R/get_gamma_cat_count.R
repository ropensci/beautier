#' Extract the gamma category count from an HKY site model
#' @param gamma_site_models one or more gamma_site_models, as created
#'   by \code{\link{create_gamma_site_model}}
#' @return the gamma category count
#' @export
get_gamma_cat_count <- function(gamma_site_models) {

  if (!is_gamma_site_model(gamma_site_models)) {
    stop("site_models must be one or more gamma_site_models")
  }
  if ("gamma_cat_count" %in% names(gamma_site_models)) {
    return(gamma_site_models$gamma_cat_count)
  }
  # The default value
  return(beastscriptr::get_default_gamma_cat_count())
}

#' Get the default gamma category count for the HKY nucleotide
#' substitution model. Use in, among others, create_hky_site_model
#' @export
get_default_gamma_cat_count <- function() {
  return(0)
}
