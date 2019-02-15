#' Determine if the object is a valid clock_model
#' @param x an object, to be determined if it is a clock_model
#' @return TRUE if the clock_model is a valid clock_model, FALSE otherwise
#' @seealso see \code{\link{create_clock_model}} for an overview of functions
#'   to create valid clock model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   testit::assert(is_clock_model(create_strict_clock_model()))
#'   testit::assert(is_clock_model(create_rln_clock_model()))
#'   testit::assert(!is_clock_model("nonsense"))
#' @export
is_clock_model <- function(
  x
) {
  if (is_rln_clock_model(x)) return(TRUE) # nolint beautier function
  if (is_strict_clock_model(x)) return(TRUE) # nolint beautier function
  FALSE
}

#' Determine if the object is a valid relaxed log normal clock model
#' @param x an object, to be determined if it is a valid
#'   relaxed log normal clock model,
#'   as created by \code{\link{create_rln_clock_model}})
#' @return TRUE if x is a valid relaxed log normal clock model, FALSE otherwise
#' @seealso \code{\link{create_clock_model}} shows an overview of
#'   functions to create a clock model
#' @author Richèl J.C. Bilderbeek
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
  tryCatch({
      check_rln_clock_model(x) # nolint beautier function
      TRUE
    },
    error = function(e) FALSE
  )
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
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_strict_clock_model <- function(
  x
) {
  tryCatch({
      check_strict_clock_model(x) # nolint beautier function
      TRUE
    },
    error = function(e) FALSE
  )
}
