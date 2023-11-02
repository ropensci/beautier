#' Check if \code{mrca_prior_name} is a valid MRCA prior name.
#'
#' A valid MRCA prior name is either \link{NA} or one character string.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return No return value, called for side effects
#' @export
check_mrca_prior_name <- function(mrca_prior_name) {
  check_string(mrca_prior_name, allow_empty = FALSE, allow_na = TRUE)
  invisible(mrca_prior_name)
}
