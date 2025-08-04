#' Determine if `x` is `TRUE`
#' @param x the object to be determined to be `TRUE`
#' @return Nothing. Will raise an exception if the value is not `TRUE`
#' @note Created by modifying check_bool from import-standalone-type-check.
#' @author [`olivroy`](https://github.com/olivroy)
#' @examples
#' check_true(TRUE)
#' @export
check_true <- function(x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {

  if (
    !missing(x) && !isFALSE(x) &&
      .standalone_types_check_dot_call(
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

#' Determine if `x` is `FALSE`
#' @param x the object to be determined to be `FALSE`
#' @return Nothing. Will raise an exception if the value is not `FALSE`
#' @note Created by modifying check_bool from import-standalone-type-check.
#' @author [`olivroy`](https://github.com/olivroy)
#' @examples
#' check_false(FALSE)
#' @export
check_false <- function(x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (
    !missing(x) && !isTRUE(x) &&
      .standalone_types_check_dot_call(
        ffi_standalone_is_bool_1.0.7, x, allow_na, allow_null)
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
