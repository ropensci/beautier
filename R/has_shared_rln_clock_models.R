#' Are there any shared RLN clock models?
#' @note this must be FALSE, as BEAUti rejects this as well
#' @author Richel J.C. Bilderbeek
has_shared_rln_clock_models <- function(
  clock_models
) {
  rln_clock_models <- clock_models[ are_rln_clock_models(clock_models)  ]
  rln_ids <- get_clock_models_ids(rln_clock_models)
  length(unique(rln_ids)) != length(rln_ids)
}
