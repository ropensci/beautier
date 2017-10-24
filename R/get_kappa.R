#' Extract the kappa from an HKY site model
#' @param site_models one or more site_models
#' @return the kappa
#' @export
get_kappa <- function(site_models) {

  if (!is_site_model(site_models)) {
    stop("site_models must be one or more site_models")
  }
  if ("kappa" %in% names(site_models)) {
    return(site_models$kappa)
  }
  # The default value
  return(beastscriptr::get_default_kappa())
}

#' Get the default kappa for the HKY nucleotide
#' substitution model. Use in, among others, create_hky_site_model
#' @note the value is returned as a string, to be sure the testing XMLs
#'   are recreated exactly, as rounding errors are prevented
#' @export
get_default_kappa <- function() {
  return("2.0")
}
