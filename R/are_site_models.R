#' Determine if x consists out of site_models objects
#' @param x the object to check if it consists out of site_models objects
#' @return TRUE if x, or all elements of x, are site_model objects
#' @examples
#'   jc69_site_model <- create_jc69_site_model()
#'   gtr_site_model <- create_gtr_site_model()
#'   both_site_models <- list(jc69_site_model, gtr_site_model)
#'   testit::assert(are_site_models(jc69_site_model))
#'   testit::assert(are_site_models(gtr_site_model))
#'   testit::assert(are_site_models(both_site_models))
#' @author Richel J.C. Bilderbeek
#' @export
are_site_models <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_site_model(x)) return(TRUE) # nolint internal function
  for (i in x) {
    if (!is_site_model(i)) return(FALSE) # nolint internal function
  }
  return(TRUE)
}
