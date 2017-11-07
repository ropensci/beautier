#' Get the distribution names
#' @return the distribution names
#' @examples
#'   names <- get_distribution_names()
#'   testthat::expect_true("uniform" %in% names)
#'   testthat::expect_true("normal" %in% names)
#' @author Richel J.C. Bilderbeek
#' @export
get_distribution_names <- function() {
  return(c("uniform", "normal"))
}
