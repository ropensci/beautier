#' Determine if x consists out of clock_models objects
#' @param x the object to check if it consists out of clock_models objects
#' @return TRUE if x, or all elements of x, are clock_model objects
#' @author Richel J.C. Bilderbeek
#' @noRd
are_clock_models <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_clock_model(x)) return(TRUE) # nolint internal function
  for (i in x) {
    if (!is_clock_model(i)) return(FALSE) # nolint internal function
  }
  TRUE
}

#' Are the clock models Relaxed Log-Normal clock models?
#' @inheritParams default_params_doc
#' @return vector of booleans with the same length
#'   as the number of clock models in \code{clock_models}.
#'   Each nth element is TRUE if the nth element
#'   in \code{clock_models} is a relaxed log-normal
#'   clock model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
are_rln_clock_models <- function(
  clock_models
) {
  testit::assert(are_clock_models(clock_models)) # nolint internal function
  rlns <- rep(NA, length(clock_models))
  for (i in seq_along(clock_models)) {
    rlns[i] <- is_rln_clock_model(clock_models[[i]]) # nolint internal function
  }
  rlns
}
