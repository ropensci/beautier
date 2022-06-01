#' Check if the \code{alignment_id} is valid.
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @examples
#' check_empty_beautier_folder()
#'
#' # anthus_aco_sub
#' alignment_id <- get_alignment_id("/home/homer/anthus_aco_sub.fas")
#' check_alignment_id(alignment_id)
#'
#' check_empty_beautier_folder()
#' @export
check_alignment_id <- function(alignment_id) {
  # An alignment ID may be uninitialized
  if (beautier::is_one_na(alignment_id)) return()

  if (length(alignment_id) != 1) {
    stop(
      "'alignment_id' must be one NA or one character string. \n",
      "Actual value: ", alignment_id
    )
  }
  if (!is.character(alignment_id)) {
    stop(
      "'alignment_id' must be one NA or one character string. \n",
      "Actual value: ", alignment_id
    )
  }
  if (nchar(alignment_id) == 0) {
    stop(
      "'alignment_id' must be one NA or one character string ",
      "of at least one character. \n",
      "Actual value: ", alignment_id
    )
  }
}
