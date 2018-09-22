#' Determine if the object is a valid clock_model
#' @param x an object, to be determined if it is a clock_model
#' @return TRUE if the clock_model is a valid clock_model, FALSE otherwise
#' @seealso see \code{\link{create_clock_model}} for an overview of functions
#'   to create valid clock model
#' @author Richel J.C. Bilderbeek
#' @noRd
is_clock_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!is_clock_model_name(x$name)) return(FALSE)
  if (!"id" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid relaxed log normal clock model
#' @param x an object, to be determined if it is a valid
#'   relaxed log normal clock model,
#'   as created by \code{\link{create_rln_clock_model}})
#' @return TRUE if x is a valid relaxed log normal clock model, FALSE otherwise
#' @seealso \code{\link{create_clock_model}} shows an overview of
#'   functions to create a clock model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rln_clock_model <- create_rln_clock_model()
#'   testit::assert(beautier:::is_rln_clock_model(rln_clock_model))
#'
#'   strict_clock_model <- create_strict_clock_model()
#'   testit::assert(beautier:::is_strict_clock_model(strict_clock_model))
#' @noRd
is_rln_clock_model <- function(
  x
) {
  if (!is_clock_model(x)) return(FALSE)
  if (x$name != "relaxed_log_normal") return(FALSE)
  if (!"ucldstdev_distr" %in% names(x)) return(FALSE)
  if (!"mean_rate_prior_distr" %in% names(x)) return(FALSE)
  if (!"mparam_id" %in% names(x)) return(FALSE)
  if (!"mean_clock_rate" %in% names(x)) return(FALSE)
  if (!"n_rate_categories" %in% names(x)) return(FALSE)
  if (!"normalize_mean_clock_rate" %in% names(x)) return(FALSE)
  if (!"dimension" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param x an object, to be determined if it is a valid strict clock model
#' @return TRUE if x is a valid strict clock model, FALSE otherwise
#' @seealso \code{\link{create_clock_model}} shows an overview of
#'   functions to create a clock model
#' @examples
#'   strict_clock_model <- create_strict_clock_model()
#'
#'   # rln: Relaxed Log-Normal
#'   rln_clock_model <- create_rln_clock_model()
#'   testit::assert(!beautier:::is_strict_clock_model(rln_clock_model))
#' @author Richel J.C. Bilderbeek
#' @noRd
is_strict_clock_model <- function(
  x
) {
  if (!is_clock_model(x)) return(FALSE)
  if (x$name != "strict") return(FALSE)
  if (!"clock_rate_param" %in% names(x)) return(FALSE)
  if (!is_param(x$clock_rate_param)) return(FALSE)
  if (!"clock_rate_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$clock_rate_distr)) return(FALSE)
  TRUE
}
