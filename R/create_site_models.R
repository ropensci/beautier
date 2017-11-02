#' Creates all supported site models
#' @return a list of site_models
#' @export
create_site_models <- function() {
  return(
    list(
      beautier::create_gtr_site_model(),
      beautier::create_hky_site_model(),
      beautier::create_jc69_site_model(),
      beautier::create_tn93_site_model()
    )
  )
}

#' Creates n jc69_site_models
#' @param n the number of jc69_site_models
#' @return a list of site_models
#' @examples
#'   m <- create_jc69_site_models(1)
#'   testthat::expect_equal(length(m), 1)
#'   testthat::expect_true(is_jc69_site_model(m[[1]]))
#'
#'   m <- create_jc69_site_models(2)
#'   testthat::expect_equal(length(m), 2)
#'   testthat::expect_true(is_jc69_site_model(m[[1]]))
#'   testthat::expect_true(is_jc69_site_model(m[[2]]))
#' @export
create_jc69_site_models <- function(n) {
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- create_jc69_site_model()
  }
  ms
}
