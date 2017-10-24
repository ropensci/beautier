#' Get the site model names
#' @return the site model names
#' @examples
#'   names <- get_site_model_names()
#'   testthat::expect_true("JC69" %in% names)
#'   testthat::expect_true("HKY" %in% names)
#'   testthat::expect_true("TN93" %in% names)
#'   testthat::expect_true("GTR" %in% names)
#' @author Richel J.C. Bilderbeek
#' @export
get_site_model_names <- function() {
  return(c("JC69", "HKY", "TN93", "GTR"))
}
