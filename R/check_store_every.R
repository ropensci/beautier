#' Check if \code{store_every} holds a valid value
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @return No return value, called for side effects
#' @export
check_store_every <- function(store_every) {
  check_number_whole(store_every, allow_na = TRUE, min = -1)
  if (is.na(store_every)) return()

  if (store_every == 0) {
    stop(
      "'store_every' must be either NA, -1 or a non-zero positive value. \n",
      "'Actual value: ", store_every
    )
  }
  invisible(store_every)
}
