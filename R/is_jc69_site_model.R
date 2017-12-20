#' Determine if the object is a valid JC69 site model
#' @param x an object, to be determined if it is a valid JC69 site model
#' @return TRUE if x is a valid JC69 site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   jc69_site_model <- create_jc69_site_model()
#'   testit::assert(beautier:::is_jc69_site_model(jc69_site_model))
is_jc69_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE)
  x$name == "JC69"
}
