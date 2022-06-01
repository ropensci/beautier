#' Determine if x consists out of clock_models objects
#' @param x the object to check if it consists out of clock_models objects
#' @return TRUE if x, or all elements of x, are clock_model objects
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' rln_clock_model <- create_rln_clock_model()
#' strict_clock_model <- create_strict_clock_model()
#' both_clock_models <- list(rln_clock_model, strict_clock_model)
#' # TRUE
#' are_clock_models(rln_clock_model)
#' are_clock_models(strict_clock_model)
#' are_clock_models(both_clock_models)
#'
#' # FALSE
#' are_clock_models(NA)
#' are_clock_models(NULL)
#' are_clock_models("nonsense")
#' are_clock_models(create_jc69_site_model())
#'
#' check_empty_beautier_folder()
#' @export
are_clock_models <- function(
  x
) {
  tryCatch({
      # We check four times. Just to check lintr-bot
      beautier::check_clock_models(x)
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
#' @export
are_rln_clock_models <- function(
  clock_models
) {
  testit::assert(beautier::are_clock_models(clock_models))
  rlns <- rep(NA, length(clock_models))
  for (i in seq_along(clock_models)) {
    rlns[i] <- beautier::is_rln_clock_model(clock_models[[i]])
  }
  rlns
}
