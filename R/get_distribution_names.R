#' Get the distribution names
#' @return the distribution names
#' @examples
#'   names <- get_distribution_names()
#'   testthat::expect_true("uniform" %in% names)
#'   testthat::expect_true("normal" %in% names)
#'   testthat::expect_true("one_div_x" %in% names)
#'   testthat::expect_true("log_normal" %in% names)
#' @author Richel J.C. Bilderbeek
#' @export
get_distribution_names <- function() {
  return(c("uniform", "normal", "one_div_x", "log_normal"))
}
