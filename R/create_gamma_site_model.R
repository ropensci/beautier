#' Create a gamma site model, part of a site model
#' @param gamma_cat_count the number of gamma categories, must
#'   be an integer with value zero or more
#' @param prop_invariant the proportion invariant, must be a value
#'   from 0.0 to 1.0
#' @return a gamma site model
#' @note Use \code{\link{create_site_model}} to create a site model
#'   that has both a gamma site model and substitution model
#' @export
create_gamma_site_model <- function(
  gamma_cat_count = get_default_gamma_cat_count(),
  prop_invariant = get_default_prop_invariant()

  ) {
  return(
    list(
      gamma_cat_count = gamma_cat_count,
      prop_invariant = prop_invariant
    )
  )
}
