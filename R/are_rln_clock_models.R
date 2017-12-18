#' Are the clock models Relaxed Log-Normal clock models?
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
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
