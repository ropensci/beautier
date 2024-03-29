#' Determine if the object is a valid clock_model
#' @param x an object, to be determined if it is a clock_model
#' @return TRUE if the clock_model is a valid clock_model, FALSE otherwise
#' @seealso see \code{\link{create_clock_model}} for an overview of functions
#'   to create valid clock model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_clock_model(create_strict_clock_model())
#' is_clock_model(create_rln_clock_model())
#'
#' # FALSE
#' is_clock_model(NA)
#' is_clock_model(NULL)
#' is_clock_model("nonsense")
#' is_clock_model(create_jc69_site_model())
#' is_clock_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_clock_model <- function(
  x
) {
  if (is_rln_clock_model(x)) return(TRUE)
  if (is_strict_clock_model(x)) return(TRUE)
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
#' check_empty_beautier_folder()
#'
#' is_rln_clock_model(create_strict_clock_model())
#' is_rln_clock_model(create_rln_clock_model())
#'
#' is_rln_clock_model(NA)
#' is_rln_clock_model(NULL)
#' is_rln_clock_model("nonsense")
#' is_rln_clock_model(create_jc69_site_model())
#' is_rln_clock_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rln_clock_model <- function(
  x
) {
  tryCatch(
    {
      check_rln_clock_model(x)
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
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_strict_clock_model(create_strict_clock_model())
#' is_strict_clock_model(create_rln_clock_model())
#'
#' is_strict_clock_model(NA)
#' is_strict_clock_model(NULL)
#' is_strict_clock_model("nonsense")
#' is_strict_clock_model(create_jc69_site_model())
#' is_strict_clock_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_strict_clock_model <- function(
  x
) {
  tryCatch(
    {
      check_strict_clock_model(x)
      TRUE
    },
    error = function(e) FALSE
  )
}
