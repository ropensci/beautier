#' Get the parameter names
#' @return the parameter names
#' @author Richel J.C. Bilderbeek
#' @examples
#'   names <- get_parameter_names()
#'   testthat::expect_true("alpha" %in% names)
#'   testthat::expect_true("beta" %in% names)
#' @export
get_parameter_names <- function() {
  return(
    c(
      "alpha",
      "beta"
    )
  )
}
