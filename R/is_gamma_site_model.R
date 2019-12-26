#' Is object x a gamma site model?
#' @param x the object to be determined if it is a valid gamma site object
#' @return TRUE if x is a valid gamma site object, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_gamma_site_model(create_gamma_site_model()))
#'
#' expect_false(is_gamma_site_model("nonsense"))
#' expect_false(is_gamma_site_model(NA))
#' expect_false(is_gamma_site_model(NULL))
#' expect_false(is_gamma_site_model(""))
#' expect_false(is_gamma_site_model(c()))
#' @export
is_gamma_site_model <- function(x) {
  tryCatch({
      beautier::check_gamma_site_model(x)
      TRUE
    },
    error = function(e) { # nolint indeed ignore e
      FALSE
    }
  )
}
