#' Determine if x consists out of clock_models objects
#' @param x the object to check if it consists out of clock_models objects
#' @return TRUE if x, or all elements of x, are clock_model objects
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rln_clock_model <- create_rln_clock_model()
#'   strict_clock_model <- create_strict_clock_model()
#'   both_clock_models <- list(rln_clock_model, strict_clock_model)
#'   testit::assert(are_clock_models(rln_clock_model))
#'   testit::assert(are_clock_models(strict_clock_model))
#'   testit::assert(are_clock_models(both_clock_models))
#' @export
are_clock_models <- function(
  x
) {
  tryCatch({
      check_clock_models(x) # nolint beautier function
      TRUE
    },
    error = function(e) FALSE
  )
}

#' Are the clock models Relaxed Log-Normal clock models?
#' @inheritParams default_params_doc
#' @return vector of booleans with the same length
#'   as the number of clock models in \code{clock_models}.
#'   Each nth element is TRUE if the nth element
#'   in \code{clock_models} is a relaxed log-normal
#'   clock model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
are_rln_clock_models <- function(
  clock_models
) {
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  rlns <- rep(NA, length(clock_models))
  for (i in seq_along(clock_models)) {
    rlns[i] <- is_rln_clock_model(clock_models[[i]]) # nolint beautier function
  }
  rlns
}
