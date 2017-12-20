#' Determine if the object is a valid clock_model
#' @param x an object, to be determined if it is a clock_model
#' @return TRUE if the clock_model is a valid clock_model, FALSE otherwise
#' @seealso see \code{\link{create_clock_model}} for an overview of functions
#'   to create valid clock model
#' @author Richel J.C. Bilderbeek
is_clock_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!is_clock_model_name(x$name)) return(FALSE)
  if (!"id" %in% names(x)) return(FALSE)
  return(TRUE)
}
