#' Get the distribution's ID
#' @param distr a distibution,
#'   as created by \code{\link{create_distr}})
#' @return the distribution's ID
#' @author Richel J.C. Bilderbeek
#' @export
get_distr_id <- function(
  distr
) {
  if (!is_distr(distr)) {
    stop("Must supply a valid distribution")
  }
  testit::assert("id" %in% names(distr))
  return(distr$id)
}
