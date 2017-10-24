#' Extract the gamma category count from an HKY site model
#' @param site_models one or more site_models
#' @return the gamma category count
#' @export
get_gamma_cat_count <- function(site_models) {

  if (!is_site_model(site_models)) {
    stop("site_models must be one or more site_models")
  }
  if ("gamma_cat_count" %in% names(site_models)) {
    return(site_models$gamma_cat_count)
  }
  # The default value
  return(get_default_gamma_cat_count())
}

#' Get the default gamma category count for the HKY nucleotide
#' substitution model. Use in, among others, create_hky_site_model
#' @export
get_default_gamma_cat_count <- function() {
  return(4)
}
