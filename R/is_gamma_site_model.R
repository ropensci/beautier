#' Is object x a gamma site model?
#' @param x the object to be determined if it is a valid gamma site object
#' @return TRUE if x is a valid gamma site object, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   gamma_site_model <- create_gamma_site_model()
#'   testit::assert(beautier:::is_gamma_site_model(gamma_site_model))
#' @noRd
is_gamma_site_model <- function(x) {
  tryCatch({
      check_gamma_site_model(x) # nolint beautier function
      TRUE
    },
    error = function(e) { # nolint indeed ignore e
      FALSE
    }
  )
}
