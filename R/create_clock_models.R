#' Creates all supported clock models
#' @return a list of clock_models
#' @export
create_clock_models <- function() {
  return(
    list(
      create_rln_clock_model(),
      create_strict_clock_model()
    )
  )
}

#' Creates n strict clock_models
#' @param n the number of strict_clock_models
#' @return a list of strict_clock objects
#' @examples
#'   m <- create_strict_clock_models(1)
#'   testthat::expect_equal(length(m), 1)
#'   testthat::expect_true(is_strict_clock_model(m[[1]]))
#'
#'   m <- create_strict_clock_models(2)
#'   testthat::expect_equal(length(m), 2)
#'   testthat::expect_true(is_strict_clock_model(m[[1]]))
#'   testthat::expect_true(is_strict_clock_model(m[[2]]))
#' @export
create_strict_clock_models <- function(n) {
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- create_strict_clock_model()
  }
  ms
}
