#' Get the distribution's ID
#' @param distribution a distibution,
#'   as created by \code{\link{create_distribution}})
#' @return the distribution's ID
#' @author Richel J.C. Bilderbeek
#' @export
get_distribution_id <- function(distribution) {
  if (!is_distribution(distribution)) {
    stop("Must supply a valid distribution")
  }
  testit::assert("id" %in% names(distribution))
  return(distribution$id)
}
