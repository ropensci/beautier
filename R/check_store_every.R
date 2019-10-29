#' Check if \code{store_every} holds a valid value
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_store_every <- function(store_every) {
  testit::assert(length(store_every) == 1)
  if (is.na(store_every)) return()
  assertive::assert_is_a_number(store_every)
  assertive::assert_all_are_whole_numbers(store_every)

  if (store_every < -1 || store_every == 0) {
    stop(
      "'store_every' must be either NA, -1 or a non-zero positive value. \n",
      "'Actual value: ", store_every
    )
  }
}
