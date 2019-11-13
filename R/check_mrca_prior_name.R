#' Check if \code{mrca_prior_name} is a valid MRCA prior name.
#'
#' A valid MRCA prior name is either \link{NA} or one character string.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @export
check_mrca_prior_name <- function(mrca_prior_name) {
  if (beautier::is_one_na(mrca_prior_name)) return()
  if (length(mrca_prior_name) != 1) {
    stop("'name' must be one NA or one character string")
  }
  if (!is.character(mrca_prior_name)) {
    stop("'name' must be one NA or one character string")
  }
  if (nchar(mrca_prior_name) == 0) {
    stop(
      "'name' must be one NA or one character string ",
      "with at least one character"
    )
  }
}
