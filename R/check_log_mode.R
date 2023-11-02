#' Check if the supplied \code{mode} is a valid logging mode.
#' @param mode mode how to log.
#' Valid are \code{tree}, \code{autodetect} and \code{compound}
#' @return No return value, called for side effects
#' @export
check_log_mode <- function(mode) {
  if (!mode %in% get_log_modes()) {
    stop(
      "'mode' must be a valid log mode. \n",
      "Supported values: '",
      paste0(get_log_modes(), collapse = ", "), "' \n",
      "Actual value: '", mode
    )
  }
  invisible(mode)
}
