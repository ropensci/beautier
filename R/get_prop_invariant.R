#' Extract the proporion invariants from an HKY site model
#' @param site_models one or more site_models
#' @return the proporion invariants
#' @export
get_prop_invariant <- function(site_models) {

  if (!is_site_model(site_models)) {
    stop("site_models must be one or more site_models")
  }
  if ("prop_invariant" %in% names(site_models)) {
    return(site_models$prop_invariant)
  }
  # The default value
  return(beastscriptr::get_default_prop_invariant())
}

#' Get the default proportion invariant for the HKY nucleotide
#' substitution model. Use in, among others, create_hky_site_model
#' @note this function returns a string to circumvent rounding
#'   errors in recreating the testing XMLs.
#' @export
get_default_prop_invariant <- function() {
  return("0.0")
}
