#' Creates all supported clock models,
#'   which is just a list of the types returned by
#'   \code{\link{create_rln_clock_model}},
#'   and \code{\link{create_strict_clock_model}}
#' @return a list of site_models
#' @author Richel J.C. Bilderbeek
#' @examples
#'  clock_models <- create_clock_models()
#'  testit::assert(is_rln_clock_model(clock_models[[1]]))
#'  testit::assert(is_strict_clock_model(clock_models[[2]]))
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
#' @param ids the alignment ids,
#'   as returned by \code{\link{get_ids}}
#' @return a list of strict_clock objects
#' @examples
#'   m <- create_strict_clock_models(ids = c("a"))
#'   testthat::expect_equal(length(m), 1)
#'   testthat::expect_true(is_strict_clock_model(m[[1]]))
#'
#'   m <- create_strict_clock_models(ids = c("a", "b"))
#'   testthat::expect_equal(length(m), 2)
#'   testthat::expect_true(is_strict_clock_model(m[[1]]))
#'   testthat::expect_true(is_strict_clock_model(m[[2]]))
#' @export
create_strict_clock_models <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_strict_clock_model(
      clock_rate_param = create_clock_rate_param(
        id = ids[i]
      )
    )
  }
  ms
}
