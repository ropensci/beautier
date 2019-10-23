#' Check if the supplied \code{sort} is a valid logging sorting option.
#' @param sort how to sort the entries in a log.
#' Valid are \code{smart}, \code{none} and \code{alphabetic}
#' @export
check_log_sort <- function(sort) {
  if (!sort %in% get_log_sorts()) {
    stop(
      "'sort' must be a valid log sort. \n",
      "Supported values: '", paste0(get_log_sorts(), collapse = ", "), "' \n",
      "Actual value: '", sort
    )
  }
}
