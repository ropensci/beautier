#' Check if the rename function is a valid filename rename function
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
check_rename_fun <- function(rename_fun) {
  if (length(rename_fun) != 1 || !is.function(rename_fun)
  ) {
    stop("'rename_fun' must be one function")
  }
  if (!is.na(rename_fun(NA))) {
    stop("'rename_fun' must return NA when given an NA")
  }
  if (length(rename_fun("some.path")) != 1 ||
      !is.character(rename_fun("some.path"))
  ) {
    stop("'rename_fun' must return a character vector with one element")
  }
}
