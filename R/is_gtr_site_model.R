#' Determine if the object is a valid GTR site model,
#' as created by \code{\link{create_gtr_site_model}}
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_gtr_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "GTR") return(FALSE)
  if (!"gamma_site_model" %in% names(x)) return(FALSE)
  TRUE
}
