#' Extract the proporion invariants from an HKY site model
#' @param gamma_site_model a gamma_site_models,
#'   as created by \code{\link{create_gamma_site_model}}
#' @return the proporion invariants
#' @export
get_prop_invariant <- function(gamma_site_model) {

  if (!is_gamma_site_model(gamma_site_model)) {
    stop("gamma_site_model must be a gamma_site_models")
  }
  testit::assert("prop_invariant" %in% names(gamma_site_model))
  gamma_site_model$prop_invariant
}

#' Get the default proportion invariant for the HKY nucleotide
#' substitution model. Use in, among others, create_hky_site_model
#' @note this function returns a string to circumvent rounding
#'   errors in recreating the testing XMLs.
#' @export
get_default_prop_invariant <- function() {
  return("0.0")
}
