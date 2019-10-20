#' Determine if x consists out of initialized clock_models objects
#' @param x the object to check if it consists out of
#'   initialized clock_models objects
#' @return TRUE if x, or all elements of x, are initialized clock_model objects
#' @author Richèl J.C. Bilderbeek
#' @export
are_init_clock_models <- function(
  x
) {
  if (!beautier::are_clock_models(x)) return(FALSE)
  for (i in x) {
    if (!beautier::is_init_clock_model(i)) return(FALSE)
  }
  TRUE
}
