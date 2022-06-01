#' Is object x a gamma site model?
#' @param x the object to be determined if it is a valid gamma site object
#' @return TRUE if x is a valid gamma site object, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_gamma_site_model(create_gamma_site_model())
#'
#' # FALSE
#' is_gamma_site_model("nonsense")
#' is_gamma_site_model(NA)
#' is_gamma_site_model(NULL)
#' is_gamma_site_model("")
#' is_gamma_site_model(c())
#'
#' check_empty_beautier_folder()
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
