#' Determine if `x` is one TRUE
#' @param x the object to be determined to be one TRUE
#' @param ... other arguments, no idea why this is needed
#' @param allow_na set to TRUE to allow NA to be valid
#' @param allow_null set to TRUE to allow NULL to be valid
#' @param arg no idea why this is needed
#' @param call no idea why this is needed
#' @return Nothing. Will raise an exception if the value is not one TRUE
#' @note From
#' [`https://github.com/r-lib/rlang`](https://github.com/r-lib/rlang),
#' file `R/import-standalone-type-check.R`
#' @author [`olivroy`](https://github.com/olivroy) and Richèl J.C. Bilderbeek
#' @examples
#' check_true(TRUE)
#' @export
check_true <- function(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (
    !missing(x)
    && !isFALSE(x)
    && .standalone_types_check_dot_call(
      ffi_standalone_is_bool_1.0.7, x, allow_na, allow_null
    )
  ) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`TRUE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' Determine if `x` is one FALSE
#' @param x the object to be determined to be one FALSE
#' @param ... other arguments, no idea why this is needed
#' @param allow_na set to TRUE to allow NA to be valid
#' @param allow_null set to TRUE to allow NULL to be valid
#' @param arg no idea why this is needed
#' @param call no idea why this is needed
#' @return Nothing. Will raise an exception if the value is not one FALSE
#' @note From
#' [`https://github.com/r-lib/rlang`](https://github.com/r-lib/rlang),
#' file `R/import-standalone-type-check.R`
#' @author [`olivroy`](https://github.com/olivroy) and Richèl J.C. Bilderbeek
#' @examples
#' check_false(FALSE)
#' @export
check_false <- function(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (
    !missing(x)
    && !isTRUE(x)
    && .standalone_types_check_dot_call(
      ffi_standalone_is_bool_1.0.7, x, allow_na, allow_null
    )
  ) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
