#' Create the deafult gamma site model
#' @return a default gamma site model
#' @author Richel J.C. Bilderbeek
#' @export
get_default_gamma_site_model <- function() {
  list(
    gamma_cat_count = get_default_gamma_cat_count(),
    prop_invariant = get_default_prop_invariant()
  )
}
